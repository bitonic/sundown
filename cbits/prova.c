#include "markdown.h"
#include "xhtml.h"
#include "buffer.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>


int
main (void)
{
  struct mkd_renderer renderer;

  char *input = "(~R∊R∘.×R)/R←1↓⍳R";

  struct buf *ob = bufnew (64);
  struct buf *ib = bufnew (strlen (input));
  
  bufputs (ib, input);

  ups_xhtml_renderer (&renderer, 0);
  ups_markdown (ob, ib, &renderer, 0xFF);
  ups_free_renderer (&renderer);

  printf ("%s", ob->data);

  bufrelease (ib);
  bufrelease (ob);

  return (0);
}
  
