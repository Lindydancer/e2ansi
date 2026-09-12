/*
 * Demonstration of "e2ansi" -- a tool
 * that exports the syntax highlighting
 * performed by Emacs to tools like "less".
 */

#include <stdio.h>

int main(void)
{
  int count;

  for (count = 0; count < 10; count++)
  {
    printf("Hello World!\n");
  }

  return 0;
}
