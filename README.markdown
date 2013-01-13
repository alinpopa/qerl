qerl is an Erlang MQ Broker based on STOMP

[![Build Status](https://api.travis-ci.org/alinpopa/qerl.png?branch=master)](https://travis-ci.org/alinpopa/qerl)

(NOTE: THIS PRODUCT IS STILL IN VERY EARLY DEVELOPMENT PHASE)

Requirements:  
  - make  
  - erlang (>R15B01)

Install by running:  

    make release

Start app by running:  

    make start / make node

Customize tcp filters:  
  - doing this by modify src/qerl\_tcp\_filters.erl (FILTERS macro).

