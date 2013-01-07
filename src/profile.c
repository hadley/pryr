// Should use sigaction instead of signal (http://linux.die.net/man/2/sigaction)

#include <R.h>
#include <Rinternals.h>
#include <Defn.h>
#include <signal.h>
#include <sys/time.h>

static FILE *R_ProfileOutfile = NULL;

// Capture call stack
static void tick(int sig) {
  RCNTXT *cptr;
  int newline = 0;

  // Not exported :(
  // unsigned long bigv, smallv, nodes;
  // get_current_mem(&smallv, &bigv, &nodes);
  // fprintf(R_ProfileOutfile, ":%ld:%ld:%ld:%ld:", smallv, bigv,
  //      nodes, get_duplicate_counter());
  // reset_duplicate_counter();

  for (cptr = R_GlobalContext; cptr; cptr = cptr->nextcontext) {
    if ((cptr->callflag & (CTXT_FUNCTION | CTXT_BUILTIN))
      && TYPEOF(cptr->call) == LANGSXP) {
      SEXP fun = CAR(cptr->call);
      if (!newline) newline = 1;
      fprintf(R_ProfileOutfile, "\"%s\" ",
          TYPEOF(fun) == SYMSXP ? CHAR(PRINTNAME(fun)) :
          "<Anonymous>");
    }
  }
  fprintf(R_ProfileOutfile, "\n");
  signal(SIGPROF, doprof);
}

static void notick(int sig) {
  signal(SIGPROF, notick);
}

static void start_profiling(SEXP filename, int append, double dinterval) {
  int interval = (int)(1e6 * dinterval + 0.5);

  R_ProfileOutfile = RC_fopen(filename, append ? "a" : "w", TRUE);
  if (R_ProfileOutfile == NULL)
    error(_("Rprof: cannot open profile file '%s'"), translateChar(filename));
  
  if (!append) {
    fprintf(R_ProfileOutfile, "sample.interval=%d\n", interval);  
  }
  
  signal(SIGPROF, tick);
  set_timer(interval);
}

void set_timer(interval) {
  struct itimerval itv;

  itv.it_interval.tv_sec = 0;
  itv.it_interval.tv_usec = interval;
  itv.it_value.tv_sec = 0;
  itv.it_value.tv_usec = interval;
  if (setitimer(ITIMER_PROF, &itv, NULL) == -1)
    error("setting profile timer failed");
}

static void end_profiling(void) {
  signal(SIGPROF, notick);
  set_timer(0);

  if(R_ProfileOutfile) {
    fclose(R_ProfileOutfile);
  }
  R_ProfileOutfile = NULL;
}
