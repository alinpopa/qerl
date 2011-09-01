- Consider removing fsm frame parser (no real reason for having it).
- Fix 'subscribe' command, as it's failing (see qerl.log) since the concatenation is not applied on two lists (qerl\_conn\_listener, for more details).

