{application, cerlan, [
    {description, "The calendar about gaming"},
    {vsn, "0.0.1"},
    {modules, [
        cerlan,
        cerlan_dispatch,
        cerlan_mochevent,
        cerlan_thome
    ]},
    {registered, []},
    {applications, [kernel, stdlib, sasl]},
    {mod, {cerlan, []}},
    {start_phases, [
      {emongo, []}
    ]}
]}.
