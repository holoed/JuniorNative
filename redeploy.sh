kn service delete junior-type
kn service delete junior-web
kn service create junior-web --image ghcr.io/holoed/juniornativeide:main
kn service create junior-type --image ghcr.io/holoed/juniornative:master --min-scale=1 --concurrency-limit=1