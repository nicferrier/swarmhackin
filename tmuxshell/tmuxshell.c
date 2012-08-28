#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

int
main (int argc, char** argv) 
{
  int swarm_session = system("tmux has-session -t swarm");
  char* attach[] = { "tmux", "attach", "-t", "swarm", NULL };
  char* new[] = { "tmux", "new", "-s", "swarm", NULL };
  if (swarm_session == 0) 
    {
      execv("/usr/bin/tmux", attach);
    }
  else 
    {
      printf("trying to new\n");
      int tmux_ret = execv("/usr/bin/tmux", new);
      printf("whoops! tmux returned with %d\n", tmux_ret);
    }
}
