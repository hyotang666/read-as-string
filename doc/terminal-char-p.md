# [Function] TERMINAL-CHAR-P

## Syntax:

(TERMINAL-CHAR-P CHARACTER) => BOOLEAN

## Description:

When CHARACTER is ansi cl supported terminal macro character or terminal space character, return T.

## Exceptional situations:

When argument is not character an error is signaled.

## Notes:

TERMINAL-CHAR-P supports only ANSI CL standard terminal macro characters, not user defined macro characters.
