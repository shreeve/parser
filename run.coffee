#!/usr/bin/env coffee

# ==============================================================================

# # helpers.extend global, require 'util'
# Parser = require './jison'
# grammar = require('/Users/shreeve/Data/Code/github/shreeve/parser/grammar')
# (new Parser(grammar)).parser.generate(moduleMain: ->)
# fs.writeFileSync 'parser-jison.js', parser

# ==============================================================================

p = console.log
# d = (obj) -> console.dir obj, depth: null, maxArrayLength: null, colors: true
d = (obj) -> console.dir obj, depth: null, maxArrayLength: null, colors: false

# ==[ CoffeeScript ]==

grammar = require './grammar'

want = process.argv[2] or 'jison'
part = process.argv[3]

switch want

  when 'jison', 'j'

    x = (require './jison').Generator
      bnf       : grammar.grammar
      operators : grammar.operators
      tokens    : grammar.tokens
      start     : grammar.start

  when 'sonar', 's'

    x = (require './sonar').Generator
      bnf       : grammar.grammar
      operators : grammar.operators
      tokens    : grammar.tokens
      start     : grammar.start

# d Object.keys(x).sort()

d (if part then x[part] else x)

# ==============================================================================
#
# symbolMap
# symbols
# terminals
# terminals_
#
# nonterminals
#
# operators
# productions
# productionTable
# startSymbol
# terminalMap
#
# computeLookaheads
# conflicts
# conflictStates
# defaultActions
# EOF
# lookahead
# moduleInclude
# onDemandLookahead
# options
# parseParams
# performAction
# resolutions
# states
# stateTable
# yy