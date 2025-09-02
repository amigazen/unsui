#include <exec/types.h>
#include <libraries/gadtools.h>
#include <intuition/intuition.h>
#include <proto/exec.h>
#include <proto/dos.h>
#include <proto/gadtools.h>
#include <proto/intuition.h>
#include "config.h"
#undef NULL
#include "lisp.h"
#include "amiga.h"

static struct Menu *emacs_menu;
static char *emacs_menu_strings;
static APTR win_vi;
struct Library *GadToolsBase;

void suspend_menus(void)
{
  if (emacs_win)
    {
      ClearMenuStrip(emacs_win);
      if (win_vi)
	{
	  FreeVisualInfo(win_vi);
	  win_vi = 0;
	}
    }
}

int resume_menus(void)
{
  if (emacs_win && emacs_menu)
    {
      win_vi = GetVisualInfo(emacs_win->WScreen, TAG_END);

      if (!win_vi || !LayoutMenus(emacs_menu, win_vi,
				  GTMN_NewLookMenus, 1L,
				  TAG_END))
	{
	  if (win_vi) FreeVisualInfo(win_vi);
	  Famiga_delete_menus();

	  return FALSE;
	}
      SetMenuStrip(emacs_win, emacs_menu);
    }
  return TRUE;
}

DEFUN ("amiga-menus", Famiga_menus, Samiga_menus, 1, 1, 0,
  "Define menus for emacs. The argument is a list structured as follows:\n\
   ((menu1-name ((item1-name item1-expr item1-key item1-disabled) ...)\n\
     menu1-disabled) ...)\n\
menu-name is the name of the menu item header.\n\
The menu is disabled if menu-disabled is not nil [optional].\n\
item-name is the name of an item.\n\
The item-expr fields are ignored.\n\
If item-key is nil, no shortcut is allowed.\n\
If item-disabled is not nil, the item is disabled.\n\
If the item information list is nil, a line is drawn in the menu.\n\
item-key & item-disabled are optional.")
  (menus)
     Lisp_Object menus;
{
    Lisp_Object s_menus, s_items;
    int citems, slen;
    char *strdata;
    struct NewMenu *menudata, *mkm;
    struct Lisp_String *name;

/*    int i;
    extern int total[], nb[];

    for (i = 0; i < 16; i++)
    {
	printf("%d(%d) ", total[i], nb[i]);
	total[i] = nb[i] = 0;
    }
    printf("\n");
    start_count(15);
    for (i = 0; i < 100; i++) { suspend_count(15); resume_count(15); }
    stop_count(15);
    for (i = 0; i < 100; i++) { start_count(14); stop_count(14); }
    printf("100 s/r: %d, 100 s/s: %d\n", total[15], total[14]);

    return Qnil;
*/
    check_intuition();

    /* Check structure of parameter & count # items & menus */
    s_menus = menus;
    citems = slen = 0;

    while (!NULL(s_menus))
    {
	struct Lisp_Cons *menu, *menu_cell;

	CHECK_CONS(s_menus, 0);
	menu_cell = XCONS(s_menus);
	citems++;
	CHECK_CONS(menu_cell->car, 0); /* Each menu is a list */
	menu = XCONS(menu_cell->car);

	CHECK_STRING(menu->car, 0); /* Check name */
	name = XSTRING(menu->car);
	slen += name->size + 1;
	CHECK_CONS(menu->cdr, 0);

	menu = XCONS(menu->cdr); /* Check items */

	s_items = menu->car;
	while (!NULL(s_items))
	{
	    struct Lisp_Cons *item, *item_cell;

	    CHECK_CONS(s_items, 0);
	    item_cell = XCONS(s_items);
	    citems++;
	    if (!NULL(item_cell->car))
	    {
		CHECK_CONS(item_cell->car, 0); /* Each item is a list */
		item = XCONS(item_cell->car);

		CHECK_STRING(item->car, 0);
		name = XSTRING(item->car);
		slen += name->size + 1;

		if (!NULL(item->cdr)) /* Only name is necessary */
		{
		    CHECK_CONS(item->cdr, 0);
		    item = XCONS(item->cdr);

		    /* Expr is arbitrary */
		    if (!NULL(item->cdr))
		    {
			CHECK_CONS(item->cdr, 0);
			item = XCONS(item->cdr);

			/* Check shortcut */
			if (!NULL(item->car))
			{
			    CHECK_NUMBER(item->car, 0);
			    slen += 2;
			}

			if (!NULL(item->cdr))
			{
			    CHECK_CONS(item->cdr, 0);
			    item = XCONS(item->cdr);

			    /* Check that end of list */
			    if (!NULL(item->cdr)) error("Badly formed item");
			}
		    }
		}
	    }
	    s_items = item_cell->cdr;
	}
	if (!NULL(menu->cdr))
	{
	    CHECK_CONS(menu->cdr, 0);
	    menu = XCONS(menu->cdr);
	    if (!NULL(menu->cdr)) error("Badly formed menu");
	}
	s_menus = menu_cell->cdr;
    }

    suspend_menus();
    if (emacs_menu) Famiga_delete_menus();

    /* Now create menu structure */
    menudata = (struct NewMenu *)alloca(sizeof(struct NewMenu) * (citems + 1));
    emacs_menu_strings = strdata = (char *)xmalloc(slen);
    mkm = menudata;
    s_menus = menus;
    while (!NULL(s_menus))
    {
	struct Lisp_Cons *menu, *menu_cell;
	struct NewMenu *menu1;

	menu_cell = XCONS(s_menus);
	mkm->nm_Type = NM_TITLE;
	menu = XCONS(menu_cell->car);
	name = XSTRING(menu->car);
	strcpy(strdata, name->data);
	mkm->nm_Label = strdata;
	strdata += name->size + 1;
	mkm->nm_CommKey = 0;
	mkm->nm_Flags = 0;
	mkm->nm_MutualExclude = 0;
	menu1 = mkm++;

	menu = XCONS(menu->cdr); /* Check items */

	s_items = menu->car;
	while (!NULL(s_items))
	{
	    struct Lisp_Cons *item, *item_cell;

	    item_cell = XCONS(s_items);
	    mkm->nm_Type = NM_ITEM;
	    mkm->nm_CommKey = 0;
	    mkm->nm_Flags = 0;
	    mkm->nm_MutualExclude = 0;
	    if (NULL(item_cell->car))
	    {
		mkm->nm_Type = IM_ITEM;
		mkm->nm_Label = NM_BARLABEL;
	    }
	    else
	    {

		item = XCONS(item_cell->car);
		name = XSTRING(item->car);
		strcpy(strdata, name->data);
		mkm->nm_Label = strdata;
		strdata += name->size + 1;

		if (!NULL(item->cdr)) /* Only name is necessary */
		{
		    item = XCONS(item->cdr);

		    /* Expr is ignored */

		    if (!NULL(item->cdr))
		    {
			item = XCONS(item->cdr);

			/* Check shortcut */
			if (!NULL(item->car))
			{
			    mkm->nm_CommKey = strdata;
			    strdata[0] = XFASTINT(item->car);
			    strdata[1] = '\0';
			    strdata += 2;
			}
			if (!NULL(item->cdr))
			{
			    item = XCONS(item->cdr);
			    if (!NULL(item->car))
				mkm->nm_Flags |= NM_ITEMDISABLED;
			}
		    }
		}
	    }
	    mkm++;
	    s_items = item_cell->cdr;
	}
	if (!NULL(menu->cdr))
	{
	    menu = XCONS(menu->cdr);
	    if (!NULL(menu->car)) menu1->nm_Flags |= NM_MENUDISABLED;
	}
	s_menus = menu_cell->cdr;
    }
    mkm->nm_Type = NM_END;
    mkm->nm_Label = 0;
    mkm->nm_CommKey = 0;
    mkm->nm_Flags = 0;
    mkm->nm_MutualExclude = 0;
    if (!(emacs_menu = CreateMenus(menudata, TAG_END)))
    {
	free(emacs_menu_strings);
	emacs_menu_strings = 0;
	error("Menu couldn't be created");
    }
    if (!resume_menus()) error("Menu couldn't be layed out");

    return Qt;
}

DEFUN ("amiga-delete-menus", Famiga_delete_menus, Samiga_delete_menus, 0, 0, 0,
       "Remove & free menu strip")
   ()
{
    check_intuition();

    suspend_menus();
    if (emacs_menu) FreeMenus(emacs_menu);
    emacs_menu = 0;
    if (emacs_menu_strings) free(emacs_menu_strings);
    emacs_menu_strings = 0;

    return Qt;
}

void syms_of_amiga_menu(void)
{
    defsubr(&Samiga_delete_menus);
    defsubr(&Samiga_menus);
}

void init_amiga_menu(void)
{
    GadToolsBase = OpenLibrary("gadtools.library", 0);
    if (!GadToolsBase) _fail("gadtools.library required");
}

void cleanup_amiga_menu(void)
{
  suspend_menus();
  if (emacs_menu) Famiga_delete_menus();
  if (GadToolsBase) CloseLibrary(GadToolsBase);
}
