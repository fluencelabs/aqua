(seq
  (call %init_peer_id% ("__magic" "load") ["relayId"] relayId)
  (seq
    (call %init_peer_id% ("__magic" "load") ["knownPeers"] knownPeers)
    (seq
      (call %init_peer_id% ("__magic" "load") ["clientId"] clientId)
      (seq
        (call relayId ("op" "identity") [])
        (par
          (seq
            (call relayId ("op" "identity") [])
            (seq
              (call relayId ("op" "identify") [] ident)
              (seq
                (call relayId ("dist" "get_blueprints") [] blueprints)
                (seq
                  (call relayId ("dist" "get_modules") [] modules)
                  (seq
                    (call relayId ("srv" "get_interfaces") [] interfaces)
                    (seq
                      (call relayId ("op" "identity") [])
                      (call %init_peer_id% ("event" "all_info") [relayId ident interfaces blueprints modules])
                      )
                    )
                  )
                )
              )
            )
          (par
            (fold knownPeers p
                  (par
                    (seq
                      (call p ("op" "identity") [])
                      (seq
                        (call p ("op" "identify") [] ident)
                        (seq
                          (call p ("dist" "get_blueprints") [] blueprints)
                          (seq
                            (call p ("dist" "get_modules") [] modules)
                            (seq
                              (call p ("srv" "get_interfaces") [] interfaces)
                              (seq
                                (call relayId ("op" "identity") [])
                                (call %init_peer_id% ("event" "all_info") [p ident interfaces blueprints modules])
                                )
                              )
                            )
                          )
                        )
                      )
                    (next p)
                    )
                  )
            (seq
              (call relayId ("op" "identity") [])
              (seq
                (call relayId ("dht" "neighborhood") [clientId] neigh)
                (fold neigh n
                      (par
                        (seq
                          (call n ("dht" "neighborhood") [clientId] moreNeigh)
                          (fold moreNeigh mp
                                (par
                                  (seq
                                    (call mp ("op" "identify") [] ident)
                                    (seq
                                      (call mp ("dist" "get_blueprints") [] blueprints)
                                      (seq
                                        (call mp ("dist" "get_modules") [] modules)
                                        (seq
                                          (call mp ("srv" "get_interfaces") [] interfaces)
                                          (seq
                                            (call relayId ("op" "identity") [])
                                            (call %init_peer_id% ("event" "all_info") [mp ident interfaces blueprints modules])
                                            )
                                          )
                                        )
                                      )
                                    )
                                  (next mp)
                                  )
                                )
                          )
                        (next n)
                        )
                      )
                )
              )
            )
          )
        )
      )
    )
  )