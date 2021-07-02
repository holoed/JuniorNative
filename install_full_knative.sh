kubectl apply -f https://github.com/knative/serving/releases/download/v0.22.0/serving-crds.yaml
sleep 5
kubectl apply -f https://github.com/knative/serving/releases/download/v0.22.0/serving-core.yaml
sleep 5
kubectl apply -f https://github.com/knative/net-kourier/releases/download/v0.22.0/kourier.yaml
sleep 5
kubectl --namespace kourier-system get service kourier
sleep 5
kubectl get pods --namespace knative-serving
sleep 5
kubectl apply -f https://github.com/knative/serving/releases/download/v0.22.0/serving-default-domain.yaml
sleep 5
export EXTERNAL_IP="127.0.0.1"
sleep 5
export KNATIVE_DOMAIN="$EXTERNAL_IP.nip.io"
sleep 5
export echo KNATIVE_DOMAIN=$KNATIVE_DOMAIN
sleep 5
dig $KNATIVE_DOMAIN
sleep 5
kubectl patch configmap -n knative-serving config-domain -p "{\"data\": {\"$KNATIVE_DOMAIN\": \"\"}}"
sleep 5
cat <<EOF | kubectl apply -f -
apiVersion: v1
kind: Service
metadata:
  name: kourier-ingress
  namespace: kourier-system
  labels:
    networking.knative.dev/ingress-provider: kourier
spec:
  type: NodePort
  selector:
    app: 3scale-kourier-gateway
  ports:
    - name: http2
      nodePort: 31080
      port: 80
      targetPort: 8080
EOF
sleep 5
kubectl patch configmap/config-network   --namespace knative-serving   --type merge   --patch '{"data":{"ingress.class":"kourier.ingress.networking.knative.dev"}}'
sleep 5
kubectl get pods -n knative-serving
sleep 5
kubectl get pods -n kourier-system
sleep 5
kubectl get svc  -n kourier-system
sleep 5
kn service create hello --image gcr.io/knative-samples/helloworld-go --port 8080 --env TARGET=Knative
sleep 5
curl http://hello.default.127.0.0.1.nip.io
sleep 5
kubectl apply -f https://github.com/knative/eventing/releases/download/v0.22.0/eventing-crds.yaml
sleep 5
kubectl apply -f https://github.com/knative/eventing/releases/download/v0.22.0/eventing-core.yaml
sleep 5
kubectl get pods --namespace knative-eventing
sleep 5
kubectl apply -f https://github.com/knative/eventing/releases/download/v0.22.0/in-memory-channel.yaml
sleep 5
kubectl apply -f https://github.com/knative/eventing/releases/download/v0.22.0/mt-channel-broker.yaml
sleep 5
kn broker create default
sleep 5

cat <<EOF | kubectl apply -f -
apiVersion: serving.knative.dev/v1
kind: Service
metadata:
  name: cloudevents-player
spec:
  template:
    metadata:
      annotations:
        autoscaling.knative.dev/minScale: "1"
    spec:
      containers:
        - image: quay.io/ruben/cloudevents-player:latest
          env:
            - name: PLAYER_MODE
              value: KNATIVE
            - name: PLAYER_BROKER
              value: default
---
apiVersion: eventing.knative.dev/v1
kind: Trigger
metadata:
  name: cloudevents-player
  annotations:
    knative-eventing-injection: enabled
spec:
  broker: default
  subscriber:
    ref:
      apiVersion: serving.knative.dev/v1
      kind: Service
      name: cloudevents-player
EOF
