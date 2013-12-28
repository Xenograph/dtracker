{application, dtracker,
	[{vsn, "1.0"},
	 {mod, {dtracker, []}},
	 {modules, [dtracker, dtracker_sup, dt_socksup, dt_listener, dt_acceptor_sup, dt_acceptor, bencode, passkey]},
	 {applications, [kernel, stdlib]}
]}.