#include <stdlib.h>

#define o_new(res) res = (struct state_t *)malloc(sizeof(struct state_t))
#define o_close(src) {                          \
    struct state_t* tmp;                        \
    o_new(tmp);                                 \
    tmp->x = src->x;                            \
    dst = tmp;                                  \
  }
#define o_incr(st,rv) { st.x += 1; rv = 0; }
#define o_clear(st,rv) { st.x = 0; rv = 0; }
#define o_decr(st,rv) { if (st.x == 0) rv = -1; else { st.x -= 1; rv = 0; } }
#define o_isz(st,rv) { rv = (st.x == 0 ? 1 : 0); }
