#define o_new(res) res = (struct state_t *)malloc(sizeof(struct state_t)); res->top = -1;

#define o_push(st,rv,v) { \
  if (st.top == (MAXSTACK-1)) {rv = 0;} \
  else { st.top++; st.a[st.top] = v; rv = 1; } }

#define o_pop(st,rv) {          \
  if (st.top == -1) rv = -1; \
  else rv = st.a[ st.top-- ]; }

#define o_isempty(st,rv) rv = (st.top==-1 ? 1 : 0)
