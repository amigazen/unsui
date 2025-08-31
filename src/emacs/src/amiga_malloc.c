/* Emulation of some unix functions for emacs.
Copyright (C) 1985, 1986, 1987, 1988 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */

#include <exec/types.h>
#include <exec/memory.h>
#include <proto/exec.h>

#undef LONGBITS
#undef NULL
#include "config.h"
#include "lisp.h"
#include "amiga.h"

/* Memory stuff */
long far DataSegBits;
static int DataSegFound;
static struct mem_header *far current;
struct mem_header *free_list;
extern int alloca_calling;
extern int *pure;
long far pre_alloc;	/* amount of memory to reserve for emacs */
char *malloc_hunk;
long malloc_hunk_size = MALLOC_HUNK_SIZE; /* Amount of memory malloc'ed by a
					     to-be-dumped emacs */
long malloc_bytes_used;	/* Amount of this hunk actually used */
static int early_malloc = TRUE;	/* Before we undump, we want system allocations */

#define ADDR_OK(x) (((long)x & ~VALMASK) == DataSegBits)

/* Memory allocation code */
/* ---------------------- */

static void *alloc_sys(long memsize)
/* Effect: Allocate from AmigaOS (via AllocMem). */
{
  /* Allocation rounded up to multiple of 4 */
  long size = ((memsize + 3) & ~3) + sizeof(struct mem_header);
  struct mem_header *mem;

  if (!DataSegFound)
    {
      /* Find page containing Pure data. All data used by emacs must be
	 on the same page (As a page is 2^26 bytes, this shouldn't be too
	 unlikely). */
      DataSegBits = (long)&first_data & ~VALMASK;
      if (!(ADDR_OK(first_fn) && ADDR_OK(last_fn) &&
	    ADDR_OK(&first_data) && ADDR_OK(&last_data) &&
	    ADDR_OK(&first_bss) && ADDR_OK(&last_bss)))
	_fail("I can't handle your memory configuration");
      DataSegFound = TRUE;
    }

  mem = AllocMem(size, 0);
  if (!mem) return 0;
  /* All memory *must* be allocated on the same page ! */
  if (!ADDR_OK(mem))
    {
      FreeMem(mem, size);
      return 0;
    }
  if (current) current->prev = mem;
  mem->next = current;
  mem->prev = 0;
  current = mem;
  mem->size = size;

  return mem + 1;
}

static void free_sys(char *p)
{
  struct mem_header *old = (struct mem_header *)p - 1;

  if (old == current)
    {
      current = current->next;
      if (current) current->prev = 0;
    }
  else
    {
      old->prev->next = old->next;
      if (old->next) old->next->prev = old->prev;
    }
  FreeMem(old, old->size);
}

int __stdargs _STD_250_EmacsMemCleanup(void)
{
  struct mem_header *next;

  while (current)
    {
      next = current->next;
      FreeMem(current, current->size);
      current = next;
    }
  return 0;
}

static void *alloc_hunk(long memsize)
/* Effect: Allocates from the malloc hunk (which is dumped to disk).
*/
{
  /* Allocation rounded up to multiple of 4 */
  long size = ((memsize + 3) & ~3) + sizeof(struct mem_header);
  /* Find a free block in the memory list */
  struct mem_header *scan = free_list->next;

  while (scan->size > 0)
    {
      if (size < scan->size)	/* Found ! */
	{
	  long end;

	  /* Split block if big enough */
	  if (size + sizeof(struct mem_header) + 8 > scan->size)
	    {
	      /* Remove block from list */
	      scan->prev->next = scan->next;
	      scan->next->prev = scan->prev;
	    }
	  else
	    {
	      /* Split block */
	      struct mem_header *new = (struct mem_header *)((char *)scan + size);

	      new->prev = scan->prev;
	      new->next = scan->next;
	      scan->prev->next = new;
	      scan->next->prev = new;
	      new->size = scan->size - size;
	      scan->size = size;
	    }
	  if (!amiga_initialized)
	    {
	      end = (char *)scan - (char *)free_list + scan->size +
		sizeof(long) + sizeof(struct mem_header);
	      if (end > malloc_bytes_used) malloc_bytes_used = end;
	    }
	  return scan + 1;
	}
      scan = scan->next;
    }
  return 0;
}

static void free_hunk(char *p)
{
  struct mem_header *old = (struct mem_header *)p - 1, *scan = free_list;

  do scan = scan->next; while (scan < old);

  /* Check for merges (potentially with both sides) */
  if ((char *)scan->prev + scan->prev->size == (char *)old)
    if ((char *)old + old->size == (char *)scan)
      {
	/* Merge all 3 blocks together */
	scan->prev->size += old->size + scan->size;
	scan->next->prev = scan->prev;
	scan->prev->next = scan->next;
      }
    else			/* Merge with previous block */
      scan->prev->size += old->size;
  else if ((char *)old + old->size == (char *)scan)
    {
      /* Merge with next block */
      old->size += scan->size;
      scan->prev->next = old;
      scan->next->prev = old;
      old->prev = scan->prev;
      old->next = scan->next;
    }
  else				/* Add a new block */
    {
      old->next = scan;
      old->prev = scan->prev;
      scan->prev->next = old;
      scan->prev = old;
    }
}

char *__halloc(long size)
{
  if (early_malloc) return alloc_sys(size);

  if (!amiga_initialized)
    if (alloca_calling)
      {
	alloca_calling = 0;
	return alloc_sys(size);
      }
    else
      {
	void *mem = alloc_hunk(size);

	if (!mem) 
	  _fail("Emacs dump: ran out of memory for malloc. \n"
		"See the -malloc option for more information.\n");
	return mem;
      }
  else
    {
      alloca_calling = 0;
      if (pre_alloc)
	{
	  void *mem;

	  if (mem = alloc_hunk(size)) return mem;
	}
      return alloc_sys(size);
    }
}

char *malloc(int size)
{
  return __halloc(size);
}

free(void *p)
{
  struct mem_header *old = (struct mem_header *)p - 1;

  if ((char *)p >= malloc_hunk &&
      (char *)p <= malloc_hunk + malloc_hunk_size + pre_alloc)
    {
      if (!amiga_initialized || pre_alloc) free_hunk(p);
    }
  else free_sys(p);
}

char *calloc(long n, long size)
{
  char *t;
  long rsize = n * size;

  t = malloc(rsize);
  if (t) memset (t, 0, rsize);

  return t;
}

char *realloc(char *p, long size)
{
  char *new = malloc(size);
  struct mem_header *old = (struct mem_header *)p - 1;

  if (new)
    {
      long minsize;
      long oldsize = old->size - sizeof(struct mem_header);

      if (size < oldsize) minsize = size;
      else minsize = oldsize;

      memcpy(new, p, minsize);
    }
  free(p);
  return new;
}

void emacs_malloc_init(void)
{
  struct mem_header *end_sentinel, *new_end, *new_block;

  early_malloc = FALSE;		/* We now have a malloc hunk */

  /* Set up the memory allocation in the malloc hunk */
  free_list = (struct mem_header *)malloc_hunk;
  end_sentinel = (struct mem_header *)((char *)free_list + malloc_hunk_size
				       - sizeof(struct mem_header));
  if (!amiga_initialized)
    {
      /* Before dumping */
      free_list->next = free_list + 1;
      free_list->prev = 0;
      free_list->size = 0;	/* Prevents merges with this pseudo-block */
      free_list[1].prev = free_list;
      free_list[1].next = end_sentinel;
      free_list[1].size =
	malloc_hunk_size - 2 * sizeof(struct mem_header) - sizeof(long);
      /* The - sizeof(long) prevents any merges with end_sentinel */

      end_sentinel->size = 0;
      end_sentinel->prev = free_list + 1;
      end_sentinel->next = 0;

      malloc_bytes_used = 0;
    }
  else if (pre_alloc)
    {
      /* After having undumped extend malloc block */
      /* Move end_sentinel: */
      new_end = (struct mem_header *)((char *)free_list + malloc_hunk_size +
				      pre_alloc - sizeof(struct mem_header));
      new_end->size = 0;
      new_end->next = 0;
      new_end->prev = end_sentinel->prev;
      end_sentinel->prev->next = new_end;

      /* Add extra memory (pre_alloc bytes) */
      new_block = (struct mem_header *)((char *)end_sentinel - sizeof(long));
      new_block->size = pre_alloc;
      free_hunk((char *)(new_block + 1));
    }
}
