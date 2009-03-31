{application, hi_pagination_app,
 [{description, "hi_pagination"},
  {vsn, "0.1"},
  {modules, [hi_pagination_sup, hi_pagination_worker, hi_pagination_app]},
  {registered, [hi_pagination]},
  {applications, [kernel, stdlib]},
  {mod, {hi_pagination_app, []}},
  {env, [{poll_for_jobs,true}]}
 ]}.