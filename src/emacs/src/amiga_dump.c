#include <exec/types.h>
#include <fcntl.h>
#include <stdio.h>
#include <assert.h>
#include <proto/dos.h>
#include <internal/messages.h>
#include "config.h"
#include "lisp.h"
#include "buffer.h"
#include "regex.h"
#include "amiga.h"
#include "dispextern.h"
#include "termchar.h"

#define RANGE(ptr, s, e) ((char *)ptr >= (char *)s && (char *)ptr < (char *)e)
#define HUNK_POS (VALBITS - 3)
#define HUNK_MASK (7 << HUNK_POS)
#define HUNK_CODE (0 << HUNK_POS)
#define HUNK_DATA (1 << HUNK_POS)
#define HUNK_BSS (2 << HUNK_POS)
#define HUNK_MALLOC (3 << HUNK_POS)
#define HUNK_PURE (4 << HUNK_POS)
#define ARRAY_MARK_FLAG ((MARKBIT >> 1) & ~MARKBIT)

void *far first_fn = first_function, *far last_fn = last_function;

extern int *pure, puresize;
extern struct gcpro *gcprolist;

extern Lisp_Object *staticvec[];
extern int staticidx;
extern struct cons_block *cons_block;
extern struct Lisp_Cons *cons_free_list;
extern struct Lisp_Vector *all_vectors;
extern struct symbol_block *symbol_block;
extern struct Lisp_Symbol *symbol_free_list;
extern struct marker_block *marker_block;
extern struct Lisp_Marker *marker_free_list;

struct string_block_head
  {
    struct string_block_head *next, *prev;
    int pos;
  };
extern struct string_block_head *current_string_block;
extern struct string_block_head *first_string_block;
extern struct string_block_head *large_string_blocks;
extern char *kbd_macro_buffer, *read_buffer, *chars_wasted, *copybuf;
extern struct minibuf_save_data *minibuf_save_vector;
extern struct re_pattern_buffer searchbuf;
extern int *ILcost, *DLcost, *ILncost, *DLncost;
extern Lisp_Object MouseMap, global_map, Vglobal_map, Vesc_map, Vctl_x_map;
extern Lisp_Object Qvariable_documentation, selected_window;

extern char *callint_argfuns[];

static void *dump_malloc(int size)
{
  void *new = malloc(size);

  if (!new) no_memory();

  return new;
}

static void bailout(char *fn)
{
  if (fn) _message("%s isn't a dump file for this version of Emacs, aborting", fn);
  else _message("Dump file isn't for this version of Emacs, aborting");

  /* We are in deep trouble, as all our variables are potentially corrupt */
  /* Therefore, no cleanup is possible */
  /* Remove cleanup routines */
  onexit(0);
  /* However, the library & the memory allocation should be ok, so
     we can exit reasonably */
  _fail("Some system resources may have been lost");
}

static void *hunk_pointer(void *ptr)
{
    if (!ptr) return ptr;

    if (RANGE(ptr, first_fn, last_fn))
	return (void *)(HUNK_CODE | (char *)ptr - (char *)first_fn);
    else if (RANGE(ptr, &first_data, &last_data))
	return (void *)(HUNK_DATA | (char *)ptr - (char *)&first_data);
    else if (RANGE(ptr, &first_bss, &last_bss))
	return (void *)(HUNK_BSS | (char *)ptr - (char *)&first_bss);
    else if (RANGE(ptr, malloc_hunk, malloc_hunk + malloc_hunk_size))
	return (void *)(HUNK_MALLOC | (char *)ptr - malloc_hunk);
    else if (RANGE(ptr, pure, (char *)pure + puresize))
	return (void *)(HUNK_PURE | (char *)ptr - (char *)pure);
    else bailout(0);
}

static Lisp_Object hunk_lispptr(Lisp_Object *objptr, Lisp_Object val)
{
    int type = val & ~VALMASK;
    void *ptr = (void *)XPNTR(val);

    if (RANGE(ptr, first_fn, last_fn))
	return type | HUNK_CODE | (char *)ptr - (char *)first_fn;
    else if (RANGE(ptr, &first_data, &last_data))
	return type | HUNK_DATA | (char *)ptr - (char *)&first_data;
    else if (RANGE(ptr, &first_bss, &last_bss))
	return type | HUNK_BSS | (char *)ptr - (char *)&first_bss;
    else if (RANGE(ptr, pure, (char *)pure + puresize))
	return type | HUNK_PURE | (char *)ptr - (char *)pure;
    else if (RANGE(ptr, malloc_hunk, malloc_hunk + malloc_hunk_size))
	return type | HUNK_MALLOC | (char *)ptr - malloc_hunk;
    else bailout(0);
}

static void patch_pointers ();

static void patch_buffer (buf)
     Lisp_Object buf;
{
  Lisp_Object tem;
  register struct buffer *buffer = XBUFFER (buf);
  register Lisp_Object *ptr;

  buffer->text.beg = hunk_pointer (buffer->text.beg);
  patch_pointers (&buffer->markers);

  /* This is the buffer's markbit */
  patch_pointers (&buffer->name);
  XMARK (buffer->name);

  for (ptr = &buffer->name + 1;
       (char *)ptr < (char *)buffer + sizeof (struct buffer);
       ptr++)
    patch_pointers (ptr);
}

static void patch_pointers (objptr)
     Lisp_Object *objptr;
{
  register Lisp_Object obj;

  obj = *objptr;
  XUNMARK (obj);


 loop:

  switch (XGCTYPE (obj))
    {
    case Lisp_String:
      *objptr = hunk_lispptr(objptr, *objptr);
      break;

    case Lisp_Vector:
    case Lisp_Window:
    case Lisp_Process:
    case Lisp_Window_Configuration:
      *objptr = hunk_lispptr(objptr, *objptr);
      {
	register struct Lisp_Vector *ptr = XVECTOR (obj);
	register int size = ptr->size;
	register int i;

	if (size & ARRAY_MARK_FLAG) break;   /* Already marked */
	ptr->size |= ARRAY_MARK_FLAG; /* Else mark it */
	for (i = 0; i < size; i++)     /* and then mark its elements */
	  patch_pointers (&ptr->contents[i]);
      }
      break;

    case Lisp_Symbol:
      *objptr = hunk_lispptr(objptr, *objptr);
      {
	register struct Lisp_Symbol *ptr = XSYMBOL (obj);
	struct Lisp_Symbol *ptrx;

	if (XMARKBIT (ptr->plist)) break;
	XMARK (ptr->plist);
	XSETTYPE (*(Lisp_Object *) &ptr->name, Lisp_String);
	patch_pointers (&ptr->name);
	patch_pointers ((Lisp_Object *) &ptr->value);
	patch_pointers (&ptr->function);
	patch_pointers (&ptr->plist);
	objptr = (Lisp_Object *)&ptr->next;
	ptr = ptr->next;
	if (ptr)
	  {
	    ptrx = ptr;		/* Use pf ptrx avoids compiler bug on Sun */
	    XSETSYMBOL (obj, ptrx);
	    goto loop;
	  }
      }
      break;

    case Lisp_Marker: {
	struct Lisp_Marker *ptr = XMARKER (obj);

	*objptr = hunk_lispptr(objptr, *objptr);
	if (XMARKBIT (ptr->chain)) break;
	XMARK (ptr->chain);
	ptr->buffer = hunk_pointer (ptr->buffer);
	patch_pointers (&ptr->chain);
	break;
    }

    case Lisp_Cons:
    case Lisp_Buffer_Local_Value:
    case Lisp_Some_Buffer_Local_Value:
      *objptr = hunk_lispptr(objptr, *objptr);
      {
	register struct Lisp_Cons *ptr = XCONS (obj);
	if (XMARKBIT (ptr->car)) break;
	XMARK (ptr->car);
	patch_pointers (&ptr->car);
	objptr = &ptr->cdr;
	obj = ptr->cdr;
	goto loop;
      }

    case Lisp_Buffer:
      *objptr = hunk_lispptr(objptr, *objptr);
      if (!XMARKBIT (XBUFFER (obj)->name))
	patch_buffer (obj);
      break;

    case Lisp_Subr: {
	struct Lisp_Subr *subr = XSUBR(obj);

	*objptr = hunk_lispptr(objptr, *objptr);
	if (subr->min_args & 0x8000) break;
	subr->min_args |= 0x8000;
	subr->function = hunk_pointer(subr->function);
	subr->symbol_name = hunk_pointer(subr->symbol_name);
	subr->prompt = hunk_pointer(subr->prompt);
	if ((long)subr->doc >= 0) /* Make sure that not a doc offset */
	    subr->doc = hunk_pointer(subr->doc);
	break;
    }

    case Lisp_Int:
    case Lisp_Void:
    case Lisp_Buffer_Objfwd: break;

    case Lisp_Intfwd:
    case Lisp_Boolfwd:
    case Lisp_Objfwd:
    case Lisp_Internal_Stream:
      *objptr = hunk_lispptr(objptr, *objptr);
    /* Don't bother with Lisp_Buffer_Objfwd,
       since all markable slots in current buffer marked anyway.  */
    /* Don't need to do Lisp_Objfwd, since the places they point
       are protected with staticpro.  */
      break;

    default:
      abort ();
    }
}

static void patch_chain(void **ptr, int offset)
{
    while (*ptr)
    {
	void **next = (void **)((char *)*ptr + offset);

	*ptr = hunk_pointer(*ptr);
	ptr = next;
    }
}

static void patch(void)
{
    int i;
    struct string_block_head *sptr;
    struct buffer *bptr;
    struct mem_header *mem;

    for (i = 0; i < staticidx; i++)
    {
	if (!XMARKBIT(*staticvec[i]))
	{
	    patch_pointers(staticvec[i]);
	    XMARK(*staticvec[i]);
	}
	staticvec[i] = hunk_pointer(staticvec[i]);
    }

    /* Patch all the pointers normally used before a dump ! */
    patch_chain((void **)&cons_block, 0);
    patch_chain((void **)&cons_free_list, 0);

    patch_chain((void **)&all_vectors, 4);

    patch_chain((void **)&symbol_block, 0);
    patch_chain((void **)&symbol_free_list, 4);

    patch_chain((void **)&marker_block, 0);
    patch_chain((void **)&marker_free_list, 4);

    /* Strings are lots of fun */
    patch_chain((void **)&large_string_blocks, 0);
    sptr = first_string_block;
    while (sptr)
    {
	struct string_block *next = sptr->next;

	if (sptr->next) sptr->next = hunk_pointer(sptr->next);
	if (sptr->prev) sptr->prev = hunk_pointer(sptr->prev);
	sptr = next;
    }
    first_string_block = hunk_pointer(first_string_block);
    current_string_block = hunk_pointer(current_string_block);

    /* More fun with buffers */
    bptr = all_buffers;
    if (bptr)
    {
	while (bptr->next)
	{
	    struct buffer *next = bptr->next;

	    bptr->next = hunk_pointer(bptr->next);
	    bptr = next;
	}
    }
    all_buffers = hunk_pointer(all_buffers);
    current_buffer = hunk_pointer(current_buffer);

    kbd_macro_buffer = hunk_pointer(kbd_macro_buffer);
    minibuf_save_vector = hunk_pointer(minibuf_save_vector);
    searchbuf.buffer = hunk_pointer(searchbuf.buffer);
    searchbuf.fastmap = hunk_pointer(searchbuf.fastmap);
    specpdl = hunk_pointer(specpdl);
    read_buffer = hunk_pointer(read_buffer);

    MouseMap = hunk_lispptr(&MouseMap, MouseMap);
    global_map = hunk_lispptr(&global_map, global_map);
    Vglobal_map = hunk_lispptr(&Vglobal_map, Vglobal_map);
    Vesc_map = hunk_lispptr(&Vesc_map, Vesc_map);
    Vctl_x_map = hunk_lispptr(&Vctl_x_map, Vctl_x_map);

    Qvariable_documentation = hunk_lispptr(&Qvariable_documentation, Qvariable_documentation);
    selected_window = hunk_lispptr(&selected_window, selected_window);

    mem = free_list;
    free_list = hunk_pointer(free_list);
    while (mem)
    {
	struct mem_header *next = mem->next;

	mem->prev = hunk_pointer(mem->prev);
	mem->next = hunk_pointer(mem->next);
	mem = next;
    }

    for (i = 0; i <= 4; i++)
      callint_argfuns[i] = hunk_pointer(callint_argfuns[i]);
}

static dump(char *fn)
{
    BPTR fd;
    long size;

    fd = Open(fn, MODE_NEWFILE);
    if (!fd)
      {
        static void unpatch();

        unpatch();
        _fail("emacs hasn't been dumped (%s missing)", fn);
      }

    Write(fd, (char *)&puresize, sizeof puresize);
    Write(fd, (char *)&malloc_hunk_size, sizeof malloc_hunk_size);
    Write(fd, (char *)&first_data, (char *)&last_data - (char *)&first_data);
    Write(fd, (char *)&first_bss, (char *)&last_bss - (char *)&first_bss);
    Write(fd, (char *)pure, puresize);
    Write(fd, (char *)malloc_hunk, malloc_hunk_size);
    Write(fd, (char *)&staticidx, sizeof staticidx);
    Write(fd, (char *)staticvec, staticidx * sizeof(Lisp_Object *));
    size = (char *)last_fn - (char *)first_fn;
    Write(fd, (char *)&size, sizeof size);

    Close(fd);
}

static void *make_pointer(void *ptr)
{
    int hunk = (long)ptr & HUNK_MASK;
    int offset = (long)ptr & (VALMASK & ~HUNK_MASK);

    if (!ptr) return 0;

    if (hunk == HUNK_CODE) return (char *)first_fn + offset;
    if (hunk == HUNK_DATA) return (char *)&first_data + offset;
    if (hunk == HUNK_BSS) return (char *)&first_bss + offset;
    if (hunk == HUNK_PURE) return (char *)pure + offset;
    if (hunk == HUNK_MALLOC) return malloc_hunk + offset;
    assert(0);
}

static Lisp_Object make_lispptr(Lisp_Object *objptr, Lisp_Object obj)
{
    long val = XUINT(obj);
    int hunk = val & HUNK_MASK;
    int offset = val & ~HUNK_MASK;
    char *ptr;

    if (hunk == HUNK_CODE) ptr = (char *)first_fn + offset;
    else if (hunk == HUNK_DATA) ptr = (char *)&first_data + offset;
    else if (hunk == HUNK_BSS) ptr = (char *)&first_bss + offset;
    else if (hunk == HUNK_PURE) ptr = (char *)pure + offset;
    else if (hunk == HUNK_MALLOC) ptr = malloc_hunk + offset;
    else assert(0);

    XSETPNTR(obj, (long)ptr);
    return obj;
}

static void unpatch_pointers ();

static void unpatch_buffer (buf)
     Lisp_Object buf;
{
  Lisp_Object tem;
  register struct buffer *buffer = XBUFFER (buf);
  register Lisp_Object *ptr;

  buffer->text.beg = make_pointer (buffer->text.beg);
  unpatch_pointers (&buffer->markers);

  /* This is the buffer's markbit */
  XUNMARK (buffer->name);
  unpatch_pointers (&buffer->name);

  for (ptr = &buffer->name + 1;
       (char *)ptr < (char *)buffer + sizeof (struct buffer);
       ptr++)
    unpatch_pointers (ptr);
}

static void unpatch_pointers (objptr)
     Lisp_Object *objptr;
{
  register Lisp_Object obj;

  obj = *objptr;
  XUNMARK (obj);


 loop:

  switch (XGCTYPE (obj))
    {
    case Lisp_String:
      *objptr = make_lispptr(objptr, *objptr);
      break;

    case Lisp_Vector:
    case Lisp_Window:
    case Lisp_Process:
    case Lisp_Window_Configuration:
      obj = *objptr = make_lispptr(objptr, *objptr);
      {
	register struct Lisp_Vector *ptr = XVECTOR (obj);
	register int size;
	register int i;

	if (!(ptr->size & ARRAY_MARK_FLAG)) break;   /* Already unmarked */
	size = ptr->size &= ~ARRAY_MARK_FLAG; /* Else unmark it */
	for (i = 0; i < size; i++)     /* and then unmark its elements */
	  unpatch_pointers (&ptr->contents[i]);
      }
      break;

    case Lisp_Symbol:
      obj = *objptr = make_lispptr(objptr, *objptr);
      {
	register struct Lisp_Symbol *ptr = XSYMBOL (obj);
	struct Lisp_Symbol *ptrx;

	if (!XMARKBIT (ptr->plist)) break;
	XUNMARK (ptr->plist);
	unpatch_pointers (&ptr->name);
	ptr->name = XSTRING (*(Lisp_Object *)&ptr->name);
	unpatch_pointers ((Lisp_Object *) &ptr->value);
	unpatch_pointers (&ptr->function);
	unpatch_pointers (&ptr->plist);
	objptr = (Lisp_Object *)&ptr->next;
	ptr = ptr->next;
	if (ptr)
	  {
	    ptrx = ptr;		/* Use pf ptrx avoids compiler bug on Sun */
	    XSET (obj, Lisp_Symbol, ptrx);
	    goto loop;
	  }
      }
      break;

    case Lisp_Marker: {
	struct Lisp_Marker *ptr;

	obj = *objptr = make_lispptr(objptr, *objptr);
	ptr = XMARKER (obj);
	if (!XMARKBIT (ptr->chain)) break;
	XUNMARK (ptr->chain);
	ptr->buffer = make_pointer (ptr->buffer);
	unpatch_pointers (&ptr->chain);
	break;
    }

    case Lisp_Cons:
    case Lisp_Buffer_Local_Value:
    case Lisp_Some_Buffer_Local_Value:
      obj = *objptr = make_lispptr(objptr, *objptr);
      {
	register struct Lisp_Cons *ptr = XCONS (obj);
	if (!XMARKBIT (ptr->car)) break;
	XUNMARK (ptr->car);
	unpatch_pointers (&ptr->car);
	objptr = &ptr->cdr;
	obj = ptr->cdr;
	goto loop;
      }

    case Lisp_Buffer:
      obj = *objptr = make_lispptr(objptr, *objptr);
      if (XMARKBIT (XBUFFER (obj)->name))
	unpatch_buffer (obj);
      break;

    case Lisp_Subr: {
	struct Lisp_Subr *subr;

	obj = *objptr = make_lispptr(objptr, *objptr);
	subr = XSUBR(obj);
	if (!(subr->min_args & 0x8000)) break;
	subr->min_args &= ~0x8000;
	subr->function = make_pointer(subr->function);
	subr->symbol_name = make_pointer(subr->symbol_name);
	subr->prompt = make_pointer(subr->prompt);
	if ((long)subr->doc >= 0) /* Make sure that not a doc offset */
	    subr->doc = make_pointer(subr->doc);
	break;
    }

    case Lisp_Int:
    case Lisp_Void:
    case Lisp_Buffer_Objfwd: break;

    case Lisp_Intfwd:
    case Lisp_Boolfwd:
    case Lisp_Objfwd:
    case Lisp_Internal_Stream:
      *objptr = make_lispptr(objptr, *objptr);
    /* Don't bother with Lisp_Buffer_Objfwd,
       since all markable slots in current buffer marked anyway.  */
    /* Don't need to do Lisp_Objfwd, since the places they point
       are protected with staticpro.  */
      break;

    default:
      abort ();
    }
}

static void unpatch_chain(void **ptr, int offset)
{
    while (*ptr)
    {
	*ptr = make_pointer(*ptr);
	ptr = (void **)((char *)*ptr + offset);
    }
}

/* Reconstructs the addresses that were patched */
static void unpatch(void)
{
    int fd, i;
    struct string_block_head *sptr;
    struct buffer *bptr;
    struct mem_header *mem;

    for (i = 0; i < staticidx; i++)
    {
	staticvec[i] = make_pointer(staticvec[i]);
	if (XMARKBIT(*staticvec[i]))
	{
	    XUNMARK(*staticvec[i]);
	    unpatch_pointers(staticvec[i]);
	}
    }

    /* Unpatch all the pointers normally used before a dump ! */
    unpatch_chain((void **)&cons_block, 0);
    unpatch_chain((void **)&cons_free_list, 0);

    unpatch_chain((void **)&all_vectors, 4);

    unpatch_chain((void **)&symbol_block, 0);
    unpatch_chain((void **)&symbol_free_list, 4);

    unpatch_chain((void **)&marker_block, 0);
    unpatch_chain((void **)&marker_free_list, 4);

    /* Strings are lots of fun */
    unpatch_chain((void **)&large_string_blocks, 0);
    sptr = first_string_block = make_pointer(first_string_block);
    current_string_block = make_pointer(current_string_block);
    while (sptr)
    {
	if (sptr->next) sptr->next = make_pointer(sptr->next);
	if (sptr->prev) sptr->prev = make_pointer(sptr->prev);
	sptr = sptr->next;
    }

    /* More fun with buffers */
    bptr = all_buffers = make_pointer(all_buffers);
    if (bptr)
    {
	while (bptr->next)
	{
	    bptr->next = make_pointer(bptr->next);
	    bptr = bptr->next;
	}
    }
    current_buffer = make_pointer(current_buffer);

    kbd_macro_buffer = make_pointer(kbd_macro_buffer);
    minibuf_save_vector = make_pointer(minibuf_save_vector);
    searchbuf.buffer = make_pointer(searchbuf.buffer);
    searchbuf.fastmap = make_pointer(searchbuf.fastmap);
    specpdl = make_pointer(specpdl);
    read_buffer = make_pointer(read_buffer);

    MouseMap = make_lispptr(&MouseMap, MouseMap);
    global_map = make_lispptr(&global_map, global_map);
    Vglobal_map = make_lispptr(&Vglobal_map, Vglobal_map);
    Vesc_map = make_lispptr(&Vesc_map, Vesc_map);
    Vctl_x_map = make_lispptr(&Vctl_x_map, Vctl_x_map);

    Qvariable_documentation = make_lispptr(&Qvariable_documentation, Qvariable_documentation);
    selected_window = make_lispptr(&selected_window, selected_window);

    free_list = make_pointer(free_list);
    mem = free_list;
    while (mem)
    {
	mem->prev = make_pointer(mem->prev);
	mem->next = make_pointer(mem->next);
	mem = mem->next;
    }

    for (i = 0; i <= 4; i++)
      callint_argfuns[i] = make_pointer(callint_argfuns[i]);
}

static undump(char *fn)
{
  BPTR fd;
  long code_size;
  char *_malloc_hunk;
  int *_pure;
  /*extern struct Library *FifoBase;
  struct Library *_FifoBase = FifoBase;*/

  fd = Open(fn, MODE_OLDFILE);
  if (!fd) return 0;

  Read(fd, (char *)&puresize, sizeof puresize);
  Read(fd, (char *)&malloc_hunk_size, sizeof malloc_hunk_size);
  _pure = dump_malloc(puresize);
  _malloc_hunk = dump_malloc(malloc_hunk_size + pre_alloc);
  Read(fd, (char *)&first_data, (char *)&last_data - (char *)&first_data);
  Read(fd, (char *)&first_bss, (char *)&last_bss - (char *)&first_bss);
  Read(fd, (char *)_pure, puresize);
  Read(fd, (char *)_malloc_hunk, malloc_hunk_size);
  Read(fd, (char *)&staticidx, sizeof staticidx);
  Read(fd, (char *)staticvec, staticidx * sizeof(Lisp_Object *));
  /*FifoBase = _FifoBase;*/
  if (Read(fd, (char *)&code_size, sizeof code_size) != sizeof code_size ||
      code_size != (char *)last_fn - (char *)first_fn)
    bailout(fn);

  Close(fd);
  malloc_hunk = _malloc_hunk;
  pure = _pure;
  return 1;
}

void map_out_data(char *fn)
{
    if (amiga_initialized) error("You can only dump once !");
    Fgarbage_collect();

    patch();
    dump(fn);
    unpatch();
    amiga_initialized = 1;
}

void map_in_data(int load)
{
    if (load && undump("GNUEmacs:etc/EMACS-DATA"))
    {
	unpatch();
	current_screen = new_screen = temp_screen = 0;
	message_buf = 0;
	chars_wasted = copybuf = 0;
	DC_ICcost = 0;
	ILcost = DLcost = ILncost = DLncost = 0;
	initialized = amiga_initialized = 1;
    }
    else
      {
	malloc_hunk = dump_malloc(malloc_hunk_size + pre_alloc);
	pure = dump_malloc(puresize);
      }
    amiga_undump_reinit();
}
