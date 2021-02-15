#include <stdlib.h>

#define o_new(res) res = (struct state_t *)malloc(sizeof(struct state_t))

#define o_clone(src) { \
  struct state_t* tmp; \
  o_new(tmp); \
  tmp->x = (src)->x;                            \
  dst = tmp; \
  }

#define o_read(st,rv)    rv = st.x
#define o_write(st,rv,v) { st.x = v; rv = 0; }


