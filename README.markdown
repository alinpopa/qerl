qerl is an Erlang MQ Broker based on STOMP

(NOTE: THIS PRODUCT IS STILL IN VERY EARLY DEVELOPMENT PHASE)

Requirements:  
  - make  
  - erlang (>R14A)

Install by running:  

    make release

Start app by running:  

    make start / make node

Customize tcp filters:  
  - doing this by modify src/qerl\_tcp\_filters.erl (FILTERS macro).

