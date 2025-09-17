#include <stdio.h>
#include "tools.h"

/* Free up the memory usde for token string */
void unmakepat(TOKEN *head)
{

	TOKEN	*old_head;

	while (head)
	{
		switch (head->tok)
		{
		case CCL:
		case NCCL:
			free(head->bitmap);
				/* fall through to default */

		default:
			old_head = head;
			head = head->next;
			free (old_head);
			break;
		}
	}
}
