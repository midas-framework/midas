# Lean Server

A fast server implementation for the Midas Web Framework.

## Notes

Lean Server is optimised for building could native servers which are behind load balancers and receive relatively* small requests responses.
This server has deliberately minimal features, so it is fast in it's designed context.

**Relatively small being pretty large on modern machines.
Requests are pulled cast into a single data structure.
The default limit to body sizes are 12MB.*
