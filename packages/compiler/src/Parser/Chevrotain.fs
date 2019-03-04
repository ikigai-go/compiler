// ts2fable 0.6.2
module rec Chevrotain
open System
open Fable.Core
open Fable.Import.JS

// let [<Import("VERSION","module")>] VERSION: string = jsNative
// let [<Import("defaultLexerErrorProvider","module")>] defaultLexerErrorProvider: ILexerErrorMessageProvider = jsNative
// let [<Import("EOF","module")>] EOF: TokenType = jsNative
// let [<Import("defaultParserErrorProvider","module")>] defaultParserErrorProvider: IParserErrorMessageProvider = jsNative
// let [<Import("defaultGrammarValidatorErrorProvider","module")>] defaultGrammarValidatorErrorProvider: IGrammarValidatorErrorMessageProvider = jsNative
// let [<Import("defaultGrammarResolverErrorProvider","module")>] defaultGrammarResolverErrorProvider: IGrammarResolverErrorMessageProvider = jsNative

type [<AllowNullLiteral>] IExports =
    abstract Parser: ParserStatic
    abstract Lexer: LexerStatic
    /// Creates a new TokenType which can then be used
    /// to define a Lexer and Parser
    abstract createToken: config: ITokenConfig -> TokenType
    /// Utility to create Chevrotain IToken "instances"
    /// Note that Chevrotain tokens are not real TokenTypes instances
    /// and thus the instanceOf cannot be used with them.
    abstract createTokenInstance: tokType: TokenType * image: string * startOffset: float * endOffset: float * startLine: float * endLine: float * startColumn: float * endColumn: float -> IToken
    abstract tokenName: tokType: TokenType -> string
    /// Returns a human readable label for a TokenType if such exists,
    /// otherwise will return the TokenType's name.
    ///
    /// Labels are useful in improving the readability of error messages and syntax diagrams.
    /// To define labels provide the label property in the {@link createToken} config parameter.
    abstract tokenLabel: tokType: TokenType -> string
    /// A Utility method to check if a token is of the type of the argument Token class.
    /// This utility is needed because Chevrotain tokens support "categories" which means
    /// A TokenType may have multiple categories.
    ///
    /// This means a simple comparison using the {@link IToken.tokenType} property may not suffice.
    /// For example:
    ///
    /// ```
    ///    import { createToken, tokenMatcher, Lexer } from "chevrotain"
    ///
    ///    // An "abstract" Token used only for categorization purposes.
    ///    const NumberTokType = createToken({ name: "NumberTokType", pattern: Lexer.NA })
    ///
    ///    const IntegerTokType = createToken({
    ///      name: "IntegerTokType",
    ///      pattern: /\d+/,
    ///      // Integer "Is A" Number
    ///      categories: [NumberTokType]
    ///    })
    ///
    ///    const DecimalTokType = createToken({
    ///      name: "DecimalTokType",
    ///      pattern: /\d+\.\d+/,
    ///      // Double "Is A" Number
    ///      categories: [NumberTokType]
    ///    })
    ///
    ///    // Will always be false as the tokenType property can only
    ///    // be Integer or Double Token Types as the Number TokenType is "abstract".
    ///    if (myToken.tokenType === NumberTokType) { /* ... *\/ }
    ///
    ///    // Will be true when myToken is of Type Integer or Double.
    ///    // Because the hierarchy defined by the categories is taken into account.
    ///    if (tokenMatcher(myToken, NumberTokType) { /* ... *\/ }
    /// ```
    abstract tokenMatcher: token: IToken * tokType: TokenType -> bool
    /// Convenience used to express an **empty** alternative in an OR (alternation).
    /// can be used to more clearly describe the intent in a case of empty alternation.
    ///
    /// For example:
    ///
    /// 1. without using EMPTY_ALT:
    ///   ```
    ///     this.OR([
    ///       {ALT: () => {
    ///         this.CONSUME1(OneTok)
    ///         return "1"
    ///       }},
    ///       {ALT: () => {
    ///         this.CONSUME1(TwoTok)
    ///         return "2"
    ///       }},
    ///       // implicitly empty because there are no invoked grammar
    ///       // rules (OR/MANY/CONSUME...) inside this alternative.
    ///       {ALT: () => {
    ///         return "666"
    ///       }},
    ///     ])
    ///   ```
    ///
    /// 2. using EMPTY_ALT:
    ///   ```
    ///     this.OR([
    ///       {ALT: () => {
    ///         this.CONSUME1(OneTok)
    ///         return "1"
    ///       }},
    ///       {ALT: () => {
    ///         this.CONSUME1(TwoTok)
    ///         return "2"
    ///       }},
    ///       // explicitly empty, clearer intent
    ///       {ALT: EMPTY_ALT("666")},
    ///     ])
    ///   ```
    abstract EMPTY_ALT: ?value: 'T -> (unit -> 'T)
    /// A utility to detect if an Error is a Chevrotain Parser's runtime exception.
    abstract isRecognitionException: error: Error -> bool
    /// An exception of this type will be saved in {@link Parser.errors} when {@link Parser.CONSUME}
    /// was called but failed to match the expected Token Type.
    abstract MismatchedTokenException: message: string * token: IToken * previousToken: IToken -> unit
    /// An exception of this type will be saved in {@link Parser.errors} when {@link Parser.OR}
    /// was called yet none of the possible alternatives could be matched.
    abstract NoViableAltException: message: string * token: IToken * previousToken: IToken -> unit
    /// An exception of this type will be saved in {@link Parser.errors} when
    /// the parser has finished yet there exists remaining input (tokens) that has not processed.
    abstract NotAllInputParsedException: message: string * token: IToken -> unit
    /// An exception of this type will be saved in {@link Parser.errors} when {@link Parser.AT_LEAST_ONE}
    /// or {@link Parser.AT_LEAST_ONE_SEP} was called but failed to match even a single iteration.
    abstract EarlyExitException: message: string * token: IToken * previousToken: IToken -> unit
    abstract GAstVisitor: GAstVisitorStatic
    abstract Rule: RuleStatic
    abstract NonTerminal: NonTerminalStatic
    abstract Flat: FlatStatic
    abstract Option: OptionStatic
    abstract RepetitionMandatory: RepetitionMandatoryStatic
    abstract RepetitionMandatoryWithSeparator: RepetitionMandatoryWithSeparatorStatic
    abstract Repetition: RepetitionStatic
    abstract RepetitionWithSeparator: RepetitionWithSeparatorStatic
    abstract Alternation: AlternationStatic
    abstract Terminal: TerminalStatic
    /// Serialize a Grammar to a JSON Object.
    ///
    /// This can be useful for scenarios requiring exporting the grammar structure
    /// for example drawing syntax diagrams.
    abstract serializeGrammar: topRules: ResizeArray<Rule> -> ResizeArray<ISerializedGast>
    /// Like {@link serializeGrammar} but for a single GAST Production instead of a set of Rules.
    abstract serializeProduction: node: IProduction -> ISerializedGast
    /// A utility to resolve a grammar AST (rules parameter).
    /// "Resolving" means assigning the appropiate value for all {@link NonTerminal.referencedRule}
    /// properties in the grammar AST.
    ///
    /// - See detailed docs for [Custom APIs](http://sap.github.io/chevrotain/docs/guide/custom_apis.html)
    abstract resolveGrammar: options: ResolveGrammarOptions -> ResizeArray<IParserDefinitionError>
    /// A utility to validate a grammar AST (rules parameter).
    /// For example: left recursion detection, ambiguity detection, ...
    ///
    /// - See detailed docs for [Custom APIs](http://sap.github.io/chevrotain/docs/guide/custom_apis.html)
    abstract validateGrammar: options: ValidateGrammarOptions -> ResizeArray<IParserDefinitionError>
    /// A utility for assigning unique occurence indices to a grammar AST (rules parameter).
    /// This can be useful when using Chevrotain to create custom APIs.
    ///
    /// - FAQ: [Why are these unique occurences needed](http://sap.github.io/chevrotain/docs/FAQ.html#NUMERICAL_SUFFIXES)
    /// - See detailed docs for [Custom APIs](http://sap.github.io/chevrotain/docs/guide/custom_apis.html)
    abstract assignOccurrenceIndices: options: AssignOccurrenceIndicesOptions -> unit
    abstract clearCache: unit -> unit
    /// Will generate an html source code (text).
    /// This html text will render syntax diagrams for the provided grammar.
    ///
    /// - See detailed docs for [Syntax Diagrams](http://sap.github.io/chevrotain/docs/guide/generating_syntax_diagrams.html).
    abstract createSyntaxDiagramsCode: grammar: ResizeArray<ISerializedGast> * ?config: ICreateSyntaxDiagramsConfig -> string
    /// Generate A Parser factory from a set of Rules.
    ///
    /// This variant will Create a factory function that once invoked with a IParserConfig will return
    /// a Parser Object.
    ///
    /// - Note that this happens using the Function constructor (a type of "eval") so it will not work in environments
    ///    where content security policy is enabled, such as certain websites, Chrome extensions ect...
    ///
    ///    This means this function is best used for development flows to reduce the feedback loops
    ///    or for productive flows targeting node.js only.
    ///
    ///    For productive flows targeting a browser runtime see {@link generateParserModule}.
    ///
    /// - See detailed docs for [Custom APIs](http://sap.github.io/chevrotain/docs/guide/custom_apis.html).
    abstract generateParserFactory: options: GenerateParserFactoryOptions -> (IParserConfig -> Parser)
    /// Generate A Parser's text from a set of Rules.
    ///
    /// This variant will generate the **string literal** for a UMD module https://github.com/umdjs/umd
    /// That exports a Parser Constructor.
    ///
    /// - Note that the constructor exposed by the generated module must receive the TokenVocabulary as the first
    ///    argument, the IParser config can be passed as the second argument.
    ///
    /// - See detailed docs for [Custom APIs](http://sap.github.io/chevrotain/docs/guide/custom_apis.html).
    abstract generateParserModule: options: GenerateParserModuleOptions -> string

type [<AllowNullLiteral>] ResolveGrammarOptions =
    abstract rules: ResizeArray<Rule> with get, set
    abstract errMsgProvider: IGrammarResolverErrorMessageProvider option with get, set

type [<AllowNullLiteral>] ValidateGrammarOptions =
    abstract rules: ResizeArray<Rule> with get, set
    /// The maximum lookahead used in the grammar.
    /// This number is needed to perform ambiguity detection.
    abstract maxLookahead: float with get, set
    /// The Token Types used by the grammar.
    abstract tokenTypes: ResizeArray<TokenType> with get, set
    abstract grammarName: string with get, set
    abstract errMsgProvider: IGrammarValidatorErrorMessageProvider with get, set
    abstract ignoredIssues: IgnoredParserIssues option with get, set

type [<AllowNullLiteral>] AssignOccurrenceIndicesOptions =
    abstract rules: ResizeArray<Rule> with get, set

type [<AllowNullLiteral>] GenerateParserFactoryOptions =
    abstract name: string with get, set
    abstract rules: ResizeArray<Rule> with get, set
    abstract tokenVocabulary: TokenVocabulary with get, set

type [<AllowNullLiteral>] GenerateParserModuleOptions =
    abstract name: string with get, set
    abstract rules: ResizeArray<Rule> with get, set

type [<AllowNullLiteral>] Parser =
    /// This must be called at the end of a Parser constructor.
    /// See: http://sap.github.io/chevrotain/docs/tutorial/step2_parsing.html#under-the-hood
    abstract performSelfAnalysis: unit -> unit
    abstract errors: ResizeArray<IRecognitionException> with get, set
    /// Resets the parser state, should be overridden for custom parsers which "carry" additional state.
    /// When overriding, remember to also invoke the super implementation!
    abstract reset: unit -> unit
    abstract getBaseCstVisitorConstructor: unit -> ParserGetBaseCstVisitorConstructorReturn
    abstract getBaseCstVisitorConstructorWithDefaults: unit -> ParserGetBaseCstVisitorConstructorWithDefaultsReturn
    abstract getGAstProductions: unit -> HashTable<Rule>
    abstract getSerializedGastProductions: unit -> ResizeArray<ISerializedGast>
    /// <param name="startRuleName"></param>
    /// <param name="precedingInput">- The token vector up to (not including) the content assist point</param>
    abstract computeContentAssist: startRuleName: string * precedingInput: ResizeArray<IToken> -> ResizeArray<ISyntacticContentAssistPath>
    /// <param name="grammarRule">- The rule to try and parse in backtracking mode.</param>
    /// <param name="args">- argumens to be passed to the grammar rule execution</param>
    abstract BACKTRACK: grammarRule: (ResizeArray<obj option> -> 'T) * ?args: ResizeArray<obj option> -> (unit -> bool)
    /// <summary>A Parsing DSL method use to consume a single Token.
    /// In EBNF terms this is equivalent to a Terminal.
    ///
    /// A Token will be consumed, IFF the next token in the token vector matches <tokType>.
    /// otherwise the parser may attempt to perform error recovery (if enabled).
    ///
    /// The index in the method name indicates the unique occurrence of a terminal consumption
    /// inside a the top level rule. What this means is that if a terminal appears
    /// more than once in a single rule, each appearance must have a **different** index.
    ///
    /// For example:
    /// ```
    ///    this.RULE("qualifiedName", () => {
    ///    this.CONSUME1(Identifier);
    ///      this.MANY(() => {
    ///        this.CONSUME1(Dot);
    ///        // here we use CONSUME2 because the terminal
    ///        // 'Identifier' has already appeared previously in the
    ///        // the rule 'parseQualifiedName'
    ///        this.CONSUME2(Identifier);
    ///      });
    ///    })
    /// ```
    ///
    /// - See more details on the [unique suffixes requirement](http://sap.github.io/chevrotain/docs/FAQ.html#NUMERICAL_SUFFIXES).</summary>
    /// <param name="tokType">- The Type of the token to be consumed.</param>
    /// <param name="options">- optional properties to modify the behavior of CONSUME.</param>
    abstract CONSUME: tokType: TokenType * ?options: ConsumeMethodOpts -> IToken
    abstract CONSUME1: tokType: TokenType * ?options: ConsumeMethodOpts -> IToken
    abstract CONSUME2: tokType: TokenType * ?options: ConsumeMethodOpts -> IToken
    abstract CONSUME3: tokType: TokenType * ?options: ConsumeMethodOpts -> IToken
    abstract CONSUME4: tokType: TokenType * ?options: ConsumeMethodOpts -> IToken
    abstract CONSUME5: tokType: TokenType * ?options: ConsumeMethodOpts -> IToken
    abstract CONSUME6: tokType: TokenType * ?options: ConsumeMethodOpts -> IToken
    abstract CONSUME7: tokType: TokenType * ?options: ConsumeMethodOpts -> IToken
    abstract CONSUME8: tokType: TokenType * ?options: ConsumeMethodOpts -> IToken
    abstract CONSUME9: tokType: TokenType * ?options: ConsumeMethodOpts -> IToken
    /// <summary>The Parsing DSL Method is used by one rule to call another.
    /// It is equivalent to a non-Terminal in EBNF notation.
    ///
    /// This may seem redundant as it does not actually do much.
    /// However using it is **mandatory** for all sub rule invocations.
    ///
    /// Calling another rule without wrapping in SUBRULE(...)
    /// will cause errors/mistakes in the Parser's self analysis phase,
    /// which will lead to errors in error recovery/automatic lookahead calculation
    /// and any other functionality relying on the Parser's self analysis
    /// output.
    ///
    /// As in CONSUME the index in the method name indicates the occurrence
    /// of the sub rule invocation in its rule.</summary>
    /// <param name="ruleToCall">- The rule to invoke.</param>
    /// <param name="options">- optional properties to modify the behavior of SUBRULE.</param>
    abstract SUBRULE: ruleToCall: (unit -> 'T) * ?options: SubruleMethodOpts -> 'T2
    abstract SUBRULE1: ruleToCall: (unit -> 'T) * ?options: SubruleMethodOpts -> 'T2
    abstract SUBRULE2: ruleToCall: (unit -> 'T) * ?options: SubruleMethodOpts -> 'T2
    abstract SUBRULE3: ruleToCall: (unit -> 'T) * ?options: SubruleMethodOpts -> 'T2
    abstract SUBRULE4: ruleToCall: (unit -> 'T) * ?options: SubruleMethodOpts -> 'T2
    abstract SUBRULE5: ruleToCall: (unit -> 'T) * ?options: SubruleMethodOpts -> 'T2
    abstract SUBRULE6: ruleToCall: (unit -> 'T) * ?options: SubruleMethodOpts -> 'T2
    abstract SUBRULE7: ruleToCall: (unit -> 'T) * ?options: SubruleMethodOpts -> 'T2
    abstract SUBRULE8: ruleToCall: (unit -> 'T) * ?options: SubruleMethodOpts -> 'T2
    abstract SUBRULE9: ruleToCall: (unit -> 'T) * ?options: SubruleMethodOpts -> 'T2
    /// <summary>Parsing DSL Method that Indicates an Optional production.
    /// in EBNF notation this is equivalent to: "[...]".
    ///
    /// Note that there are two syntax forms:
    /// - Passing the grammar action directly:
    ///    ```
    ///      this.OPTION(() => {
    ///        this.CONSUME(Digit)}
    ///      );
    ///    ```
    ///
    /// - using an "options" object:
    ///    ```
    ///      this.OPTION({
    ///        GATE:predicateFunc,
    ///        DEF: () => {
    ///          this.CONSUME(Digit)
    ///      }});
    ///    ```
    ///
    /// The optional 'GATE' property in "options" object form can be used to add constraints
    /// to invoking the grammar action.
    ///
    /// As in CONSUME the index in the method name indicates the occurrence
    /// of the optional production in it's top rule.</summary>
    /// <param name="actionORMethodDef">- The grammar action to optionally invoke once
    ///    or an "OPTIONS" object describing the grammar action and optional properties.</param>
    abstract OPTION: actionORMethodDef: (unit -> 'OUT) -> 'OUT
    abstract OPTION1: actionORMethodDef: (unit -> 'OUT) -> 'OUT
    abstract OPTION2: actionORMethodDef: (unit -> 'OUT) -> 'OUT
    abstract OPTION3: actionORMethodDef: (unit -> 'OUT) -> 'OUT
    abstract OPTION4: actionORMethodDef: (unit -> 'OUT) -> 'OUT
    abstract OPTION5: actionORMethodDef: (unit -> 'OUT) -> 'OUT
    abstract OPTION6: actionORMethodDef: (unit -> 'OUT) -> 'OUT
    abstract OPTION7: actionORMethodDef: (unit -> 'OUT) -> 'OUT
    abstract OPTION8: actionORMethodDef: (unit -> 'OUT) -> 'OUT
    abstract OPTION9: actionORMethodDef: (unit -> 'OUT) -> 'OUT
    /// <summary>Parsing DSL method that indicates a choice between a set of alternatives must be made.
    /// This is equivalent to an EBNF alternation (A | B | C | D ...), except
    /// that the alternatives are ordered like in a PEG grammar.
    /// This means that the **first** matching alternative is always chosen.
    ///
    /// There are several forms for the inner alternatives array:
    ///
    /// - Passing alternatives array directly:
    ///    ```
    ///      this.OR([
    ///        { ALT:() => { this.CONSUME(One) }},
    ///        { ALT:() => { this.CONSUME(Two) }},
    ///        { ALT:() => { this.CONSUME(Three) }}
    ///      ])
    ///    ```
    ///
    /// - Passing alternative array directly with predicates (GATE):
    ///    ```
    ///      this.OR([
    ///        { GATE: predicateFunc1, ALT:() => { this.CONSUME(One) }},
    ///        { GATE: predicateFuncX, ALT:() => { this.CONSUME(Two) }},
    ///        { GATE: predicateFuncX, ALT:() => { this.CONSUME(Three) }}
    ///      ])
    ///    ```
    ///
    /// - These syntax forms can also be mixed:
    ///    ```
    ///      this.OR([
    ///        {
    ///          GATE: predicateFunc1,
    ///          ALT:() => { this.CONSUME(One) }
    ///        },
    ///        { ALT:() => { this.CONSUME(Two) }},
    ///        { ALT:() => { this.CONSUME(Three) }}
    ///      ])
    ///    ```
    ///
    /// - Additionally an "options" object may be used:
    ///    ```
    ///      this.OR({
    ///        DEF:[
    ///          { ALT:() => { this.CONSUME(One) }},
    ///          { ALT:() => { this.CONSUME(Two) }},
    ///          { ALT:() => { this.CONSUME(Three) }}
    ///        ],
    ///        // OPTIONAL property
    ///        ERR_MSG: "A Number"
    ///      })
    ///    ```
    ///
    /// The 'predicateFuncX' in the long form can be used to add constraints to choosing the alternative.
    ///
    /// As in CONSUME the index in the method name indicates the occurrence
    /// of the alternation production in it's top rule.</summary>
    /// <param name="altsOrOpts">- A set of alternatives or an "OPTIONS" object describing the alternatives and optional properties.</param>
    abstract OR: altsOrOpts:  {| ALT: unit -> 'T |}[] -> 'T
    abstract OR1: altsOrOpts: {| ALT: unit -> 'T |}[] -> 'T
    abstract OR2: altsOrOpts: {| ALT: unit -> 'T |}[] -> 'T
    abstract OR3: altsOrOpts: {| ALT: unit -> 'T |}[] -> 'T
    abstract OR4: altsOrOpts: {| ALT: unit -> 'T |}[] -> 'T
    abstract OR5: altsOrOpts: {| ALT: unit -> 'T |}[] -> 'T
    abstract OR6: altsOrOpts: {| ALT: unit -> 'T |}[] -> 'T
    abstract OR7: altsOrOpts: {| ALT: unit -> 'T |}[] -> 'T
    abstract OR8: altsOrOpts: {| ALT: unit -> 'T |}[] -> 'T
    abstract OR9: altsOrOpts: {| ALT: unit -> 'T |}[] -> 'T
    /// <summary>Parsing DSL method, that indicates a repetition of zero or more.
    /// This is equivalent to EBNF repetition {...}.
    ///
    /// Note that there are two syntax forms:
    /// - Passing the grammar action directly:
    ///    ```
    ///      this.MANY(() => {
    ///        this.CONSUME(Comma)
    ///        this.CONSUME(Digit)
    ///       })
    ///    ```
    ///
    /// - using an "options" object:
    ///    ```
    ///      this.MANY({
    ///        GATE: predicateFunc,
    ///        DEF: () => {
    ///               this.CONSUME(Comma)
    ///               this.CONSUME(Digit)
    ///             }
    ///      });
    ///    ```
    ///
    /// The optional 'GATE' property in "options" object form can be used to add constraints
    /// to invoking the grammar action.
    ///
    /// As in CONSUME the index in the method name indicates the occurrence
    /// of the repetition production in it's top rule.</summary>
    /// <param name="actionORMethodDef">- The grammar action to optionally invoke multiple times
    ///     or an "OPTIONS" object describing the grammar action and optional properties.</param>
    abstract MANY: actionORMethodDef: (unit->unit) -> unit
    abstract MANY1: actionORMethodDef: (unit->unit) -> unit
    abstract MANY2: actionORMethodDef: (unit->unit) -> unit
    abstract MANY3: actionORMethodDef: (unit->unit) -> unit
    abstract MANY4: actionORMethodDef: (unit->unit) -> unit
    abstract MANY5: actionORMethodDef: (unit->unit) -> unit
    abstract MANY6: actionORMethodDef: (unit->unit) -> unit
    abstract MANY7: actionORMethodDef: (unit->unit) -> unit
    abstract MANY8: actionORMethodDef: (unit->unit) -> unit
    abstract MANY9: actionORMethodDef: (unit->unit) -> unit
    /// <summary>Parsing DSL method, that indicates a repetition of zero or more with a separator
    /// Token between the repetitions.
    ///
    /// Example:
    ///
    /// ```
    ///      this.MANY_SEP({
    ///          SEP:Comma,
    ///          DEF: () => {
    ///              this.CONSUME(Number};
    ///              // ...
    ///          })
    /// ```
    ///
    /// Note that because this DSL method always requires more than one argument the options object is always required
    /// and it is not possible to use a shorter form like in the MANY DSL method.
    ///
    /// Note that for the purposes of deciding on whether or not another iteration exists
    /// Only a single Token is examined (The separator). Therefore if the grammar being implemented is
    /// so "crazy" to require multiple tokens to identify an item separator please use the more basic DSL methods
    /// to implement it.
    ///
    /// As in CONSUME the index in the method name indicates the occurrence
    /// of the repetition production in it's top rule.
    ///
    /// Note that due to current limitations in the implementation the "SEP" property must appear **before** the "DEF" property.</summary>
    /// <param name="options">- An object defining the grammar of each iteration and the separator between iterations</param>
    abstract MANY_SEP: options: ManySepMethodOpts<obj option> -> unit
    abstract MANY_SEP1: options: ManySepMethodOpts<obj option> -> unit
    abstract MANY_SEP2: options: ManySepMethodOpts<obj option> -> unit
    abstract MANY_SEP3: options: ManySepMethodOpts<obj option> -> unit
    abstract MANY_SEP4: options: ManySepMethodOpts<obj option> -> unit
    abstract MANY_SEP5: options: ManySepMethodOpts<obj option> -> unit
    abstract MANY_SEP6: options: ManySepMethodOpts<obj option> -> unit
    abstract MANY_SEP7: options: ManySepMethodOpts<obj option> -> unit
    abstract MANY_SEP8: options: ManySepMethodOpts<obj option> -> unit
    abstract MANY_SEP9: options: ManySepMethodOpts<obj option> -> unit
    /// <summary>Convenience method, same as MANY but the repetition is of one or more.
    /// failing to match at least one repetition will result in a parsing error and
    /// cause a parsing error.</summary>
    /// <param name="actionORMethodDef">- The grammar action to optionally invoke multiple times
    ///   or an "OPTIONS" object describing the grammar action and optional properties.</param>
    abstract AT_LEAST_ONE: actionORMethodDef: U2<GrammarAction<obj option>, DSLMethodOptsWithErr<obj option>> -> unit
    abstract AT_LEAST_ONE1: actionORMethodDef: U2<GrammarAction<obj option>, DSLMethodOptsWithErr<obj option>> -> unit
    abstract AT_LEAST_ONE2: actionORMethodDef: U2<GrammarAction<obj option>, DSLMethodOptsWithErr<obj option>> -> unit
    abstract AT_LEAST_ONE3: actionORMethodDef: U2<GrammarAction<obj option>, DSLMethodOptsWithErr<obj option>> -> unit
    abstract AT_LEAST_ONE4: actionORMethodDef: U2<GrammarAction<obj option>, DSLMethodOptsWithErr<obj option>> -> unit
    abstract AT_LEAST_ONE5: actionORMethodDef: U2<GrammarAction<obj option>, DSLMethodOptsWithErr<obj option>> -> unit
    abstract AT_LEAST_ONE6: actionORMethodDef: U2<GrammarAction<obj option>, DSLMethodOptsWithErr<obj option>> -> unit
    abstract AT_LEAST_ONE7: actionORMethodDef: U2<GrammarAction<obj option>, DSLMethodOptsWithErr<obj option>> -> unit
    abstract AT_LEAST_ONE8: actionORMethodDef: U2<GrammarAction<obj option>, DSLMethodOptsWithErr<obj option>> -> unit
    abstract AT_LEAST_ONE9: actionORMethodDef: U2<GrammarAction<obj option>, DSLMethodOptsWithErr<obj option>> -> unit
    /// <summary>Convenience method, same as MANY_SEP but the repetition is of one or more.
    /// failing to match at least one repetition will result in a parsing error and
    /// cause the parser to attempt error recovery.
    ///
    /// Note that an additional optional property ERR_MSG can be used to provide custom error messages.</summary>
    /// <param name="options">- An object defining the grammar of each iteration and the separator between iterations</param>
    abstract AT_LEAST_ONE_SEP: options: AtLeastOneSepMethodOpts<obj option> -> unit
    abstract AT_LEAST_ONE_SEP1: options: AtLeastOneSepMethodOpts<obj option> -> unit
    abstract AT_LEAST_ONE_SEP2: options: AtLeastOneSepMethodOpts<obj option> -> unit
    abstract AT_LEAST_ONE_SEP3: options: AtLeastOneSepMethodOpts<obj option> -> unit
    abstract AT_LEAST_ONE_SEP4: options: AtLeastOneSepMethodOpts<obj option> -> unit
    abstract AT_LEAST_ONE_SEP5: options: AtLeastOneSepMethodOpts<obj option> -> unit
    abstract AT_LEAST_ONE_SEP6: options: AtLeastOneSepMethodOpts<obj option> -> unit
    abstract AT_LEAST_ONE_SEP7: options: AtLeastOneSepMethodOpts<obj option> -> unit
    abstract AT_LEAST_ONE_SEP8: options: AtLeastOneSepMethodOpts<obj option> -> unit
    abstract AT_LEAST_ONE_SEP9: options: AtLeastOneSepMethodOpts<obj option> -> unit
    /// <param name="name">- The name of the rule.</param>
    /// <param name="implementation">- The implementation of the rule.</param>
    /// <param name="config">- The rule's optional configuration.</param>
    abstract RULE: name: string * implementation: (ResizeArray<obj option> -> 'T) * ?config: IRuleConfig<'T> -> (float -> ResizeArray<obj option> -> U2<'T, obj option>)
    /// Same as {@link Parser.RULE}, but should only be used in
    /// "extending" grammars to override rules/productions from the super grammar.
    /// See [Parser Inheritance Example](https://github.com/SAP/chevrotain/tree/master/examples/parser/inheritance).
    abstract OVERRIDE_RULE: name: string * impl: (ResizeArray<obj option> -> 'T) * ?config: IRuleConfig<'T> -> (float -> ResizeArray<obj option> -> 'T)
    /// Returns an "imaginary" Token to insert when Single Token Insertion is done
    /// Override this if you require special behavior in your grammar.
    /// For example if an IntegerToken is required provide one with the image '0' so it would be valid syntactically.
    abstract getTokenToInsert: tokType: TokenType -> IToken
    /// By default all tokens type may be inserted. This behavior may be overridden in inheriting Recognizers
    /// for example: One may decide that only punctuation tokens may be inserted automatically as they have no additional
    /// semantic value. (A mandatory semicolon has no additional semantic meaning, but an Integer may have additional meaning
    /// depending on its int value and context (Inserting an integer 0 in cardinality: "[1..]" will cause semantic issues
    /// as the max of the cardinality will be greater than the min value (and this is a false error!).
    abstract canTokenTypeBeInsertedInRecovery: tokType: TokenType -> bool
    abstract getNextPossibleTokenTypes: grammarPath: ITokenGrammarPath -> ResizeArray<TokenType>
    abstract input: ResizeArray<IToken> with get, set
    abstract SKIP_TOKEN: unit -> IToken
    abstract LA: howMuch: float -> IToken

type [<AllowNullLiteral>] ParserGetBaseCstVisitorConstructorReturn =
    [<Emit "new $0($1...)">] abstract Create: [<ParamArray>] args: ResizeArray<obj option> -> Parser

type [<AllowNullLiteral>] ParserGetBaseCstVisitorConstructorWithDefaultsReturn =
    [<Emit "new $0($1...)">] abstract Create: [<ParamArray>] args: ResizeArray<obj option> -> Parser

type [<AllowNullLiteral>] ParserStatic =
    abstract performSelfAnalysis: parserInstance: Parser -> unit
    /// <summary>It is recommended to reuse the same Parser instance
    /// by passing an empty array to the input argument
    /// and only later setting the input by using the input property.
    /// See: http://sap.github.io/chevrotain/docs/FAQ.html#major-performance-benefits</summary>
    /// <param name="tokenVocabulary">- A data structure containing all the Tokens used by the Parser.</param>
    [<Emit "new $0($1...)">] abstract Create: tokenVocabulary: TokenVocabulary * ?config: IParserConfig -> Parser

type [<RequireQualifiedAccess>] ParserDefinitionErrorType =
    | INVALID_RULE_NAME = 0
    | DUPLICATE_RULE_NAME = 1
    | INVALID_RULE_OVERRIDE = 2
    | DUPLICATE_PRODUCTIONS = 3
    | UNRESOLVED_SUBRULE_REF = 4
    | LEFT_RECURSION = 5
    | NONE_LAST_EMPTY_ALT = 6
    | AMBIGUOUS_ALTS = 7
    | CONFLICT_TOKENS_RULES_NAMESPACE = 8
    | INVALID_TOKEN_NAME = 9
    | INVALID_NESTED_RULE_NAME = 10
    | DUPLICATE_NESTED_NAME = 11
    | NO_NON_EMPTY_LOOKAHEAD = 12
    | AMBIGUOUS_PREFIX_ALTS = 13
    | TOO_MANY_ALTS = 14

type [<AllowNullLiteral>] ILexerDefinitionError =
    abstract message: string with get, set
    abstract ``type``: LexerDefinitionErrorType with get, set
    abstract tokenTypes: ResizeArray<TokenType> option with get, set

type [<AllowNullLiteral>] Lexer =
    abstract lexerDefinitionErrors: ResizeArray<ILexerDefinitionError> with get, set
    /// <summary>Will lex(Tokenize) a string.
    /// Note that this can be called repeatedly on different strings as this method
    /// does not modify the state of the Lexer.</summary>
    /// <param name="text">- The string to lex</param>
    /// <param name="initialMode">- The initial Lexer Mode to start with, by default this will be the first mode in the lexer's
    ///             definition. If the lexer has no explicit modes it will be the implicit single 'default_mode' mode.</param>
    abstract tokenize: text: string * ?initialMode: string -> ILexingResult

type [<AllowNullLiteral>] LexerStatic =
    abstract SKIPPED: string with get, set
    /// A Constant to mark "abstract" TokenTypes that are used
    /// purely as token categories.
    /// See: {@link createToken.categories}
    abstract NA: RegExp with get, set
    /// <param name="lexerDefinition">-
    /// Structure composed of Tokens Types this lexer will identify.
    ///
    /// In the simple case the structure is an array of TokenTypes.
    /// In the case of {</param>
    [<Emit "new $0($1...)">] abstract Create: lexerDefinition: U2<ResizeArray<TokenType>, IMultiModeLexerDefinition> * ?config: ILexerConfig -> Lexer

type [<AllowNullLiteral>] ILexingResult =
    abstract tokens: ResizeArray<IToken> with get, set
    abstract groups: TypeLiteral_01 with get, set
    abstract errors: ResizeArray<ILexingError> with get, set

type [<AllowNullLiteral>] ILexingError =
    abstract offset: float with get, set
    abstract line: float with get, set
    abstract column: float with get, set
    abstract length: float with get, set
    abstract message: string with get, set

type [<AllowNullLiteral>] ILexerConfig =
    /// An optional flag indicating that lexer definition errors
    /// should not automatically cause an error to be raised.
    /// This can be useful when wishing to indicate lexer errors in another manner
    /// than simply throwing an error (for example in an online playground).
    abstract deferDefinitionErrorsHandling: bool option with get, set
    /// "full" location information means all six combinations of /(end|start)(Line|Column|Offset)/ properties.
    /// "onlyStart" means that only startLine, startColumn and startOffset will be tracked
    /// "onlyOffset" means that only the startOffset will be tracked.
    ///
    /// The less position tracking the faster the Lexer will be and the less memory used.
    /// However the difference is not large (~10% On V8), thus reduced location tracking options should only be used
    /// in edge cases where every last ounce of performance is needed.
    abstract positionTracking: U3<string, string, string> option with get, set
    /// A regExp defining custom line terminators.
    /// This will be used to calculate the line and column information.
    ///
    /// Note that the regExp should use the global flag, for example: /\n/g
    ///
    /// The default is: /\n|\r\n?/g
    ///
    /// But some grammars have a different definition, for example in ECMAScript:
    /// https://www.ecma-international.org/ecma-262/8.0/index.html#sec-line-terminators
    /// U+2028 and U+2029 are also treated as line terminators.
    ///
    /// In that case we would use /\n|\r|\u2028|\u2029/g
    ///
    /// Note that it is also possible to supply an optimized RegExp like implementation
    /// as only a subset of the RegExp APIs is needed, {@link ILineTerminatorsTester}
    /// for details.
    ///
    /// keep in mind that for the default pattern: /\n|\r\n?/g an optimized implementation is already built-in.
    /// This means the optimization is only relevant for lexers overriding the default pattern.
    abstract lineTerminatorsPattern: U2<RegExp, ILineTerminatorsTester> option with get, set
    /// Characters or CharCodes that represent line terminators for this lexer.
    /// This always needs to be provided when using a custom {@link ILexerConfig.lineTerminatorsPattern}.
    /// In the future this duplication may be removed or reduced.
    abstract lineTerminatorCharacters: ResizeArray<U2<float, string>> option with get, set
    /// When true this flag will cause the Lexer to throw an Error
    /// When it is unable to perform all of its performance optimizations.
    ///
    /// In addition error messages will be printed to the console with details
    /// how to resolve the optimizations issues.
    ///
    /// Use this flag to guarantee higher lexer performance.
    /// The optimizations can boost the lexer's performance anywhere from 30%
    /// to 100%+ depending on the number of TokenTypes used.
    abstract ensureOptimizations: bool option with get, set
    /// Can be used to disable lexer optimizations
    /// If there is a suspicion they are causing incorrect behavior.
    /// Note that this would have negative performance implications.
    abstract safeMode: bool option with get, set
    /// A custom error message provider.
    /// Can be used to override the default error messages.
    /// For example:
    ///    - Translating the error messages to a different languages.
    ///    - Changing the formatting.
    abstract errorMessageProvider: ILexerErrorMessageProvider option with get, set

type [<AllowNullLiteral>] ILexerErrorMessageProvider =
    /// <summary>An Unexpected Character Error occurs when the lexer is unable to match a range of one or more
    /// characters in the input text against any of the Token Types in it's Lexer definition</summary>
    /// <param name="fullText">- Full original input text.</param>
    /// <param name="startOffset">- Offset in input text where error starts.</param>
    /// <param name="length">- Error length.</param>
    /// <param name="line">- Line number where the error occured. (optional)
    ///      Will not be provided when lexer is not defined to track lines/columns</param>
    /// <param name="column">- Column number where the error occured. (optional)
    ///   Will not be provided when lexer is not defined to track lines/columns</param>
    abstract buildUnexpectedCharactersMessage: fullText: string * startOffset: float * length: float * ?line: float * ?column: float -> string
    /// <summary>Unable To Pop Lexer Mode Error happens when lexer tries to pop the last remaining mode from the mode stack
    /// so that there is no longer any active lexer mode
    /// This error only relevant for multi-mode lexers</summary>
    /// <param name="token">- The Token that requested pop mode.</param>
    abstract buildUnableToPopLexerModeMessage: token: IToken -> string

/// A subset of the regExp interface.
/// Needed to compute line/column info by a chevrotain lexer.
type [<AllowNullLiteral>] ILineTerminatorsTester =
    /// Just like regExp.test
    abstract test: (string -> bool) with get, set
    /// Just like the regExp lastIndex with the global flag enabled
    /// It should be updated after every match to point to the offset where the next
    /// match attempt starts.
    abstract lastIndex: float with get, set

type [<AllowNullLiteral>] ITokenConfig =
    abstract name: string with get, set
    /// Categories enable polymorphism on Token Types.
    /// A TokenType X with categories C1, C2, ... ,Cn can
    /// be matched by the parser against any of those categories.
    /// In practical terms this means that:
    /// CONSUME(C1) can match a Token of type X.
    abstract categories: U2<TokenType, ResizeArray<TokenType>> option with get, set
    /// The Label is a human readable name to be used
    /// in error messages and syntax diagrams.
    ///
    /// For example a TokenType may be called LCurly, which is
    /// short for "left curly brace". The much easier to understand
    /// label could simply be "{".
    abstract label: string option with get, set
    /// This defines what sequence of characters would be matched
    /// To this TokenType when Lexing.
    ///
    /// For Custom Patterns see: http://sap.github.io/chevrotain/docs/guide/custom_token_patterns.html
    abstract pattern: U4<RegExp, string, CustomPatternMatcherFunc, ICustomPattern> option with get, set
    /// The group property will cause the lexer to collect
    /// Tokens of this type separately from the other Tokens.
    ///
    /// For example this could be used to collect comments for
    /// post processing.
    ///
    /// See: https://github.com/SAP/chevrotain/tree/master/examples/lexer/token_groups
    abstract group: string option with get, set
    /// A name of a Lexer mode to "enter" once this Token Type has been matched.
    /// Lexer modes can be used to support different sets of possible Tokens Types
    ///
    /// Lexer Modes work as a stack of Lexers, so "entering" a mode means pushing it to the top of the stack.
    ///
    /// See: https://github.com/SAP/chevrotain/tree/master/examples/lexer/multi_mode_lexer
    abstract push_mode: string option with get, set
    /// If "pop_mode" is true the Lexer will pop the last mode of the modes stack and
    /// continue lexing using the new mode at the top of the stack.
    ///
    /// See: https://github.com/SAP/chevrotain/tree/master/examples/lexer/multi_mode_lexer
    abstract pop_mode: bool option with get, set
    /// The "longer_alt" property will cause the Lexer to attempt matching against another Token Type
    /// every time this Token Type has been matched.
    ///
    /// This feature can be useful when two Token Types have common prefixes which
    /// cannot be resolved (only) by the ordering of the Tokens in the lexer definition.
    ///
    /// For example see: https://github.com/SAP/chevrotain/tree/master/examples/lexer/keywords_vs_identifiers
    /// For resolving the keywords vs Identifier ambiguity.
    abstract longer_alt: TokenType option with get, set
    /// Can a String matching this Token Type's pattern possibly contain a line terminator?
    /// If true and the line_breaks property is not also true this will cause inaccuracies in the Lexer's line / column tracking.
    abstract line_breaks: bool option with get, set
    /// Possible starting characters or charCodes of the pattern.
    /// These will be used to optimize the Lexer's performance.
    ///
    /// These are normally **automatically** computed, however the option to explicitly
    /// specify those can enable optimizations even when the automatic analysis fails.
    ///
    /// e.g:
    ///    * strings hints should be one character long.
    ///     ```
    ///       { start_chars_hint: ["a", "b"] }
    ///     ```
    ///
    ///    * number hints are the result of running ".charCodeAt(0)" on the strings.
    ///     ```
    ///       { start_chars_hint: [97, 98] }
    ///     ```
    ///
    ///    * For unicode characters outside the BMP use the first of their surrogate pairs.
    ///      for example: The '💩' character is represented by surrogate pairs: '\uD83D\uDCA9'
    ///        and D83D is 55357 in decimal.
    ///     * Note that "💩".charCodeAt(0) === 55357
    abstract start_chars_hint: ResizeArray<U2<string, float>> option with get, set

type [<AllowNullLiteral>] CustomPatternMatcherFunc =
    [<Emit "$0($1...)">] abstract Invoke: test: string * offset: float * ?tokens: ResizeArray<IToken> * ?groups: CustomPatternMatcherFuncInvokeGroups -> RegExpExecArray option

type [<AllowNullLiteral>] CustomPatternMatcherFuncInvokeGroups =
    [<Emit "$0[$1]{{=$2}}">] abstract Item: groupName: string -> ResizeArray<IToken> with get, set

type [<AllowNullLiteral>] TokenType =
    abstract name: string with get, set
    abstract GROUP: string option with get, set
    abstract PATTERN: U2<RegExp, string> option with get, set
    abstract LABEL: string option with get, set
    abstract LONGER_ALT: TokenType option with get, set
    abstract POP_MODE: bool option with get, set
    abstract PUSH_MODE: string option with get, set
    abstract LINE_BREAKS: bool option with get, set
    abstract CATEGORIES: ResizeArray<TokenType> option with get, set
    abstract tokenName: string option with get, set
    abstract tokenTypeIdx: float option with get, set
    abstract categoryMatches: ResizeArray<float> option with get, set
    abstract categoryMatchesMap: TypeLiteral_02 option with get, set
    abstract isParent: bool option with get, set

/// API #2 for [Custom Token Patterns](http://sap.github.io/chevrotain/docs/guide/custom_token_patterns.html).
type [<AllowNullLiteral>] ICustomPattern =
    abstract exec: CustomPatternMatcherFunc with get, set

/// Things to note:
///   - The offset range is inclusive to exclusive.
///
/// - A lineTerminator as the last character does not effect the Token's line numbering.
///    In other words a new line only starts **after** a line terminator.
///
/// - A Token's image is it's **literal** text.
///    e.g unicode escaping is untouched.
type [<AllowNullLiteral>] IToken =
    /// The textual representation of the Token as it appeared in the text.
    abstract image: string with get, set
    /// Offset of the first character of the Token.
    abstract startOffset: int with get, set
    /// Line of the first character of the Token.
    abstract startLine: int with get, set
    /// Column of the first character of the Token.
    abstract startColumn: int with get, set
    /// Offset of the last character of the Token.
    abstract endOffset: int with get, set
    /// Line of the last character of the Token.
    abstract endLine: int with get, set
    /// Column of the last character of the Token.
    abstract endColumn: int with get, set
    /// this marks if a Token does not really exist and has been inserted "artificially" during parsing in rule error recovery.
    abstract isInsertedInRecovery: bool with get, set
    /// An number index representing the type of the Token use <getTokenConstructor> to get the Token Type from a token "instance"
    abstract tokenTypeIdx: float with get, set
    /// The actual Token Type of this Token "instance"
    /// This is the same Object returned by the "createToken" API.
    /// This property is very useful for debugging the Lexing and Parsing phases.
    abstract tokenType: TokenType with get, set

type [<AllowNullLiteral>] MultiModesDefinition =
    [<Emit "$0[$1]{{=$2}}">] abstract Item: modeName: string -> ResizeArray<TokenType> with get, set

type [<AllowNullLiteral>] IMultiModeLexerDefinition =
    abstract modes: MultiModesDefinition with get, set
    abstract defaultMode: string with get, set

type [<AllowNullLiteral>] TokenTypeDictionary =
    [<Emit "$0[$1]{{=$2}}">] abstract Item: tokenName: string -> TokenType with get, set

type TokenVocabulary =
    U3<TokenTypeDictionary, ResizeArray<TokenType>, IMultiModeLexerDefinition>

type [<AllowNullLiteral>] IRuleConfig<'T> =
    /// The function which will be invoked to produce the returned value for a production that have not been
    /// successfully executed and the parser recovered from.
    abstract recoveryValueFunc: (unit -> 'T) option with get, set
    /// Enable/Disable re-sync error recovery for this specific production.
    abstract resyncEnabled: bool option with get, set

type [<AllowNullLiteral>] DSLMethodOpts<'T> =
    /// in-lined method name
    abstract NAME: string option with get, set
    /// The Grammar to process in this method.
    abstract DEF: GrammarAction<'T> with get, set
    /// A semantic constraint on this DSL method
    abstract GATE: (unit -> bool) option with get, set

type [<AllowNullLiteral>] DSLMethodOptsWithErr<'T> =
    inherit DSLMethodOpts<'T>
    /// Short title/classification to what is being matched.
    /// Will be used in the error message,.
    /// If none is provided, the error message will include the names of the expected
    /// Tokens sequences which start the method's inner grammar
    abstract ERR_MSG: string option with get, set

type [<AllowNullLiteral>] OrMethodOpts<'T> =
    abstract NAME: string option with get, set
    /// The set of alternatives,
    /// See detailed description in {@link Parser.OR}
    abstract DEF: ResizeArray<IAnyOrAlt<'T>> with get, set
    /// A description for the alternatives used in error messages
    /// If none is provided, the error message will include the names of the expected
    /// Tokens sequences which may start each alternative.
    abstract ERR_MSG: string option with get, set

type [<AllowNullLiteral>] ManySepMethodOpts<'T> =
    abstract NAME: string option with get, set
    /// The grammar to process in each iteration.
    abstract DEF: GrammarAction<'T> with get, set
    /// The separator between each iteration.
    abstract SEP: TokenType with get, set

type [<AllowNullLiteral>] AtLeastOneSepMethodOpts<'T> =
    inherit ManySepMethodOpts<'T>
    /// Short title/classification to what is being matched.
    /// Will be used in the error message,.
    /// If none is provided, the error message will include the names of the expected
    /// Tokens sequences which start the method's inner grammar.
    abstract ERR_MSG: string option with get, set

type [<AllowNullLiteral>] ConsumeMethodOpts =
    /// A custom Error message if the Token could not be consumed.
    /// This will override any error message provided by the parser's "errorMessageProvider"
    abstract ERR_MSG: string option with get, set
    /// A label to be used instead of the TokenType name in the created CST.
    abstract LABEL: string option with get, set

type [<AllowNullLiteral>] SubruleMethodOpts =
    /// The arguments to parameterized rules, see:
    /// https://github.com/SAP/chevrotain/blob/master/examples/parser/parametrized_rules/parametrized.js
    abstract ARGS: ResizeArray<obj option> option with get, set
    /// A label to be used instead of the subrule's name in the created CST.
    abstract LABEL: string option with get, set

type [<AllowNullLiteral>] GrammarAction<'OUT> =
    [<Emit "$0($1...)">] abstract Invoke: unit -> 'OUT

type [<AllowNullLiteral>] HashTable<'V> =
    interface end

type IAnyOrAlt<'T> =
    U2<IOrAlt<'T>, IOrAltWithGate<'T>>

/// ```
///     $.OR([
///       {ALT:XXX },
///       {ALT:YYY },
///       {ALT:ZZZ }
///     ])
/// ```
type [<AllowNullLiteral>] IOrAlt<'T> =
    abstract NAME: string option with get, set
    abstract ALT: (unit -> 'T) with get, set

/// ```
///   $.OR([
///     { GATE:condition1, ALT:XXX },
///     { GATE:condition2, ALT:YYY },
///     { GATE:condition3, ALT:ZZZ }
///   ])
/// ```
type [<AllowNullLiteral>] IOrAltWithGate<'T> =
    inherit IOrAlt<'T>
    abstract NAME: string option with get, set
    abstract GATE: (unit -> bool) with get, set
    abstract ALT: (unit -> 'T) with get, set

type [<AllowNullLiteral>] ICstVisitor<'IN, 'OUT> =
    abstract visit: cstNode: U2<CstNode, ResizeArray<CstNode>> * ?param: 'IN -> 'OUT
    abstract validateVisitor: unit -> unit

/// A [Concrete Syntax Tree](http://sap.github.io/chevrotain/docs/guide/concrete_syntax_tree.html) Node.
/// This structure represents the whole parse tree of the grammar
/// This means that information on each and every Token is present.
/// This is unlike an AST (Abstract Syntax Tree) where some of the syntactic information is missing.
///
/// For example given an ECMAScript grammar, an AST would normally not contain information on the location
/// of Commas, Semi colons, redundant parenthesis ect, however a CST would have that information.
type [<AllowNullLiteral>] CstNode =
    abstract name: string
    abstract children: CstChildrenDictionary
    abstract recoveredNode: bool option
    /// Only relevant for [in-lined](http://sap.github.io/chevrotain/docs/guide/concrete_syntax_tree.html#in-lined-rules) rules.
    /// the fullName will **also** include the name of the top level rule containing this nested rule.
    abstract fullName: string option

type [<AllowNullLiteral>] CstChildrenDictionary =
    [<Emit "$0[$1]{{=$2}}">] abstract Item: identifier: string -> ResizeArray<CstElement> with get, set

type CstElement =
    U2<IToken, CstNode>

type [<AllowNullLiteral>] IParserConfig =
    /// Is the error recovery / fault tolerance of the Chevrotain Parser enabled.
    abstract recoveryEnabled: bool option with get, set
    /// Maximum number of tokens the parser will use to choose between alternatives.
    abstract maxLookahead: float option with get, set
    /// Used to mark parser definition errors that should be ignored.
    /// For example:
    ///
    /// ```
    ///     {
    ///       myCustomRule : {
    ///                       OR3 : true
    ///                      },
    ///       myOtherRule : {
    ///                      OPTION1 : true,
    ///                      OR4 : true
    ///                     }
    ///     }
    /// ```
    ///
    /// Be careful when ignoring errors, they are usually there for a reason :).
    abstract ignoredIssues: IgnoredParserIssues option with get, set
    /// Enable This Flag to to support Dynamically defined Tokens.
    /// This will disable performance optimizations which cannot work if the whole Token vocabulary is not known
    /// During Parser initialization.
    ///
    /// See [runnable example](https://github.com/SAP/chevrotain/tree/master/examples/parser/dynamic_tokens)
    abstract dynamicTokensEnabled: bool option with get, set
    /// Enable automatic Concrete Syntax Tree creation
    /// For in-depth docs on [Concrete Syntax Trees](http://sap.github.io/chevrotain/docs/guide/concrete_syntax_tree.html):
    abstract outputCst: bool option with get, set
    /// A custom error message provider.
    /// Can be used to override the default error messages.
    /// For example:
    ///    - Translating the error messages to a different languages.
    ///    - Changing the formatting.
    ///    - Providing special error messages under certain conditions, e.g: missing semicolons.
    abstract errorMessageProvider: IParserErrorMessageProvider option with get, set
    abstract serializedGrammar: ResizeArray<ISerializedGast> option with get, set

type [<AllowNullLiteral>] IgnoredParserIssues =
    [<Emit "$0[$1]{{=$2}}">] abstract Item: ruleName: string -> IgnoredRuleIssues with get, set

type [<AllowNullLiteral>] IgnoredRuleIssues =
    [<Emit "$0[$1]{{=$2}}">] abstract Item: dslNameAndOccurrence: string -> bool with get, set

type [<AllowNullLiteral>] IParserErrorMessageProvider =
    /// Mismatched Token Error happens when the parser attempted to consume a terminal and failed.
    /// It corresponds to a failed {@link Parser.CONSUME} in Chevrotain DSL terms.
    abstract buildMismatchTokenMessage: options: IParserErrorMessageProviderBuildMismatchTokenMessageOptions -> string
    /// A Redundant Input Error happens when the parser has completed parsing but there
    /// is still unprocessed input remaining.
    abstract buildNotAllInputParsedMessage: options: IParserErrorMessageProviderBuildNotAllInputParsedMessageOptions -> string
    /// A No Viable Alternative Error happens when the parser cannot detect any valid alternative in an alternation.
    /// It corresponds to a failed {@link Parser.OR} in Chevrotain DSL terms.
    abstract buildNoViableAltMessage: options: IParserErrorMessageProviderBuildNoViableAltMessageOptions -> string
    /// An Early Exit Error happens when the parser cannot detect the first mandatory iteration of a repetition.
    /// It corresponds to a failed {@link Parser.AT_LEAST_ONE} or {@link Parser.AT_LEAST_ONE_SEP} in Chevrotain DSL terms.
    abstract buildEarlyExitMessage: options: IParserErrorMessageProviderBuildEarlyExitMessageOptions -> string

type [<AllowNullLiteral>] IParserErrorMessageProviderBuildMismatchTokenMessageOptions =
    abstract expected: TokenType with get, set
    abstract actual: IToken with get, set
    abstract previous: IToken with get, set
    abstract ruleName: string with get, set

type [<AllowNullLiteral>] IParserErrorMessageProviderBuildNotAllInputParsedMessageOptions =
    abstract firstRedundant: IToken with get, set
    abstract ruleName: string with get, set

type [<AllowNullLiteral>] IParserErrorMessageProviderBuildNoViableAltMessageOptions =
    abstract expectedPathsPerAlt: ResizeArray<ResizeArray<ResizeArray<TokenType>>> with get, set
    abstract actual: ResizeArray<IToken> with get, set
    abstract previous: IToken with get, set
    abstract customUserDescription: string with get, set
    abstract ruleName: string with get, set

type [<AllowNullLiteral>] IParserErrorMessageProviderBuildEarlyExitMessageOptions =
    abstract expectedIterationPaths: ResizeArray<ResizeArray<TokenType>> with get, set
    abstract actual: ResizeArray<IToken> with get, set
    abstract previous: IToken with get, set
    abstract customUserDescription: string with get, set
    abstract ruleName: string with get, set

type [<AllowNullLiteral>] IRecognizerContext =
    /// A copy of the parser's rule stack at the "time" the RecognitionException occurred.
    /// This can be used to help debug parsing errors (How did we get here?).
    abstract ruleStack: ResizeArray<string> with get, set
    /// A copy of the parser's rule occurrence stack at the "time" the RecognitionException occurred.
    /// This can be used to help debug parsing errors (How did we get here?).
    abstract ruleOccurrenceStack: ResizeArray<float> with get, set

type [<AllowNullLiteral>] ISeparatedIterationResult<'OUT> =
    abstract values: ResizeArray<'OUT> with get, set
    abstract separators: ResizeArray<IToken> with get, set

type [<AllowNullLiteral>] ISerializedGast =
    abstract ``type``: obj with get, set
    abstract definition: ResizeArray<ISerializedGast> option with get, set

/// Structure for the path the parser "took" to reach a certain position
/// in the grammar.
type [<AllowNullLiteral>] IGrammarPath =
    /// The Grammar rules invoked and still unterminated to reach this Grammar Path.
    abstract ruleStack: ResizeArray<string> with get, set
    /// The occurrence index (SUBRULE1/2/3/5/...) of each Grammar rule invoked and still unterminated.
    /// Used to distinguish between **different** invocations of the same subrule at the same top level rule.
    abstract occurrenceStack: ResizeArray<float> with get, set

type [<AllowNullLiteral>] ISyntacticContentAssistPath =
    inherit IGrammarPath
    abstract nextTokenType: TokenType with get, set
    abstract nextTokenOccurrence: float with get, set

type [<AllowNullLiteral>] ITokenGrammarPath =
    inherit IGrammarPath
    abstract lastTok: TokenType with get, set
    abstract lastTokOccurrence: float with get, set

type [<RequireQualifiedAccess>] LexerDefinitionErrorType =
    | MISSING_PATTERN = 0
    | INVALID_PATTERN = 1
    | EOI_ANCHOR_FOUND = 2
    | UNSUPPORTED_FLAGS_FOUND = 3
    | DUPLICATE_PATTERNS_FOUND = 4
    | INVALID_GROUP_TYPE_FOUND = 5
    | PUSH_MODE_DOES_NOT_EXIST = 6
    | MULTI_MODE_LEXER_WITHOUT_DEFAULT_MODE = 7
    | MULTI_MODE_LEXER_WITHOUT_MODES_PROPERTY = 8
    | MULTI_MODE_LEXER_DEFAULT_MODE_VALUE_DOES_NOT_EXIST = 9
    | LEXER_DEFINITION_CANNOT_CONTAIN_UNDEFINED = 10
    | SOI_ANCHOR_FOUND = 11
    | EMPTY_MATCH_PATTERN = 12
    | NO_LINE_BREAKS_FLAGS = 13
    | UNREACHABLE_PATTERN = 14
    | IDENTIFY_TERMINATOR = 15
    | CUSTOM_LINE_BREAK = 16

/// A Chevrotain Parser runtime exception.
type [<AllowNullLiteral>] IRecognitionException =
    abstract name: string with get, set
    abstract message: string with get, set
    /// The token which caused the parser error.
    abstract token: IToken with get, set
    /// Additional tokens which have been re-synced in error recovery due to the original error.
    /// This information can be used the calculate the whole text area which has been skipped due to an error.
    /// For example for displaying with a red underline in a text editor.
    abstract resyncedTokens: ResizeArray<IToken> with get, set
    abstract context: IRecognizerContext with get, set

type [<AllowNullLiteral>] IOptionallyNamedProduction =
    abstract name: string option with get, set

type [<AllowNullLiteral>] IProduction =
    abstract accept: visitor: IGASTVisitor -> unit

type [<AllowNullLiteral>] IProductionWithOccurrence =
    inherit IProduction
    abstract idx: float with get, set

/// A very basic implementation of a Visitor Pattern
/// For the Grammar AST structure.
///
/// This may be useful for advanced users who create custom logic on the grammar AST.
/// For example, custom validations or introspection.
type [<AllowNullLiteral>] GAstVisitor =
    abstract visit: node: IProduction -> obj option
    abstract visitNonTerminal: node: NonTerminal -> obj option
    abstract visitFlat: node: Flat -> obj option
    abstract visitOption: node: Option -> obj option
    abstract visitRepetition: node: Repetition -> obj option
    abstract visitRepetitionMandatory: node: RepetitionMandatory -> obj option
    abstract visitRepetitionMandatoryWithSeparator: node: RepetitionMandatoryWithSeparator -> obj option
    abstract visitRepetitionWithSeparator: node: RepetitionWithSeparator -> obj option
    abstract visitAlternation: node: Alternation -> obj option
    abstract visitTerminal: node: Terminal -> obj option
    abstract visitRule: node: Rule -> obj option

/// A very basic implementation of a Visitor Pattern
/// For the Grammar AST structure.
///
/// This may be useful for advanced users who create custom logic on the grammar AST.
/// For example, custom validations or introspection.
type [<AllowNullLiteral>] GAstVisitorStatic =
    [<Emit "new $0($1...)">] abstract Create: unit -> GAstVisitor

/// The Grammar AST class representing a top level {@link Parser.RULE} call.
type [<AllowNullLiteral>] Rule =
    abstract name: string with get, set
    abstract orgText: string with get, set
    abstract definition: ResizeArray<IProduction> with get, set
    abstract accept: visitor: IGASTVisitor -> unit

/// The Grammar AST class representing a top level {@link Parser.RULE} call.
type [<AllowNullLiteral>] RuleStatic =
    [<Emit "new $0($1...)">] abstract Create: options: RuleStaticOptions -> Rule

type [<AllowNullLiteral>] RuleStaticOptions =
    abstract name: string with get, set
    abstract definition: ResizeArray<IProduction> with get, set
    abstract orgText: string option with get, set

/// The Grammar AST class representing a top level {@link Parser.SUBRULE} call.
type [<AllowNullLiteral>] NonTerminal =
    inherit IProductionWithOccurrence
    abstract nonTerminalName: string with get, set
    abstract referencedRule: Rule with get, set
    abstract idx: float with get, set
    abstract definition: ResizeArray<IProduction> with get, set
    abstract accept: visitor: IGASTVisitor -> unit

/// The Grammar AST class representing a top level {@link Parser.SUBRULE} call.
type [<AllowNullLiteral>] NonTerminalStatic =
    [<Emit "new $0($1...)">] abstract Create: options: NonTerminalStaticOptions -> NonTerminal

type [<AllowNullLiteral>] NonTerminalStaticOptions =
    abstract nonTerminalName: string with get, set
    abstract referencedRule: Rule option with get, set
    abstract idx: float option with get, set

/// The Grammar AST class used to represent a sequence.
/// This is normally only used in {@link Alternation} to distinguish
/// between the different alternatives.
type [<AllowNullLiteral>] Flat =
    inherit IOptionallyNamedProduction
    abstract name: string with get, set
    abstract definition: ResizeArray<IProduction> with get, set
    abstract accept: visitor: IGASTVisitor -> unit

/// The Grammar AST class used to represent a sequence.
/// This is normally only used in {@link Alternation} to distinguish
/// between the different alternatives.
type [<AllowNullLiteral>] FlatStatic =
    [<Emit "new $0($1...)">] abstract Create: options: FlatStaticOptions -> Flat

type [<AllowNullLiteral>] FlatStaticOptions =
    abstract definition: ResizeArray<IProduction> with get, set
    abstract name: string option with get, set

/// The Grammar AST class representing a {@link Parser.OPTION} call.
type [<AllowNullLiteral>] Option =
    inherit IProductionWithOccurrence
    inherit IOptionallyNamedProduction
    abstract idx: float with get, set
    abstract name: string option with get, set
    abstract definition: ResizeArray<IProduction> with get, set
    abstract accept: visitor: IGASTVisitor -> unit

/// The Grammar AST class representing a {@link Parser.OPTION} call.
type [<AllowNullLiteral>] OptionStatic =
    [<Emit "new $0($1...)">] abstract Create: options: OptionStaticOptions -> Option

type [<AllowNullLiteral>] OptionStaticOptions =
    abstract definition: ResizeArray<IProduction> with get, set
    abstract idx: float option with get, set
    abstract name: string option with get, set

/// The Grammar AST class representing a {@link Parser.AT_LEAST_ONE} call.
type [<AllowNullLiteral>] RepetitionMandatory =
    inherit IProductionWithOccurrence
    inherit IOptionallyNamedProduction
    abstract name: string with get, set
    abstract idx: float with get, set
    abstract definition: ResizeArray<IProduction> with get, set
    abstract accept: visitor: IGASTVisitor -> unit

/// The Grammar AST class representing a {@link Parser.AT_LEAST_ONE} call.
type [<AllowNullLiteral>] RepetitionMandatoryStatic =
    [<Emit "new $0($1...)">] abstract Create: options: RepetitionMandatoryStaticOptions -> RepetitionMandatory

type [<AllowNullLiteral>] RepetitionMandatoryStaticOptions =
    abstract definition: ResizeArray<IProduction> with get, set
    abstract idx: float option with get, set
    abstract name: string option with get, set

/// The Grammar AST class representing a {@link Parser.AT_LEAST_ONE_SEP} call.
type [<AllowNullLiteral>] RepetitionMandatoryWithSeparator =
    inherit IProductionWithOccurrence
    inherit IOptionallyNamedProduction
    abstract separator: TokenType with get, set
    abstract idx: float with get, set
    abstract name: string with get, set
    abstract definition: ResizeArray<IProduction> with get, set
    abstract accept: visitor: IGASTVisitor -> unit

/// The Grammar AST class representing a {@link Parser.AT_LEAST_ONE_SEP} call.
type [<AllowNullLiteral>] RepetitionMandatoryWithSeparatorStatic =
    [<Emit "new $0($1...)">] abstract Create: options: RepetitionMandatoryWithSeparatorStaticOptions -> RepetitionMandatoryWithSeparator

type [<AllowNullLiteral>] RepetitionMandatoryWithSeparatorStaticOptions =
    abstract definition: ResizeArray<IProduction> with get, set
    abstract separator: TokenType with get, set
    abstract idx: float option with get, set
    abstract name: string option with get, set

/// The Grammar AST class representing a {@link Parser.MANY} call.
type [<AllowNullLiteral>] Repetition =
    inherit IProductionWithOccurrence
    inherit IOptionallyNamedProduction
    abstract separator: TokenType with get, set
    abstract idx: float with get, set
    abstract name: string with get, set
    abstract definition: ResizeArray<IProduction> with get, set
    abstract accept: visitor: IGASTVisitor -> unit

/// The Grammar AST class representing a {@link Parser.MANY} call.
type [<AllowNullLiteral>] RepetitionStatic =
    [<Emit "new $0($1...)">] abstract Create: options: RepetitionStaticOptions -> Repetition

type [<AllowNullLiteral>] RepetitionStaticOptions =
    abstract definition: ResizeArray<IProduction> with get, set
    abstract idx: float option with get, set
    abstract name: string option with get, set

/// The Grammar AST class representing a {@link Parser.MANY_SEP} call.
type [<AllowNullLiteral>] RepetitionWithSeparator =
    inherit IProductionWithOccurrence
    inherit IOptionallyNamedProduction
    abstract separator: TokenType with get, set
    abstract idx: float with get, set
    abstract name: string with get, set
    abstract definition: ResizeArray<IProduction> with get, set
    abstract accept: visitor: IGASTVisitor -> unit

/// The Grammar AST class representing a {@link Parser.MANY_SEP} call.
type [<AllowNullLiteral>] RepetitionWithSeparatorStatic =
    [<Emit "new $0($1...)">] abstract Create: options: RepetitionWithSeparatorStaticOptions -> RepetitionWithSeparator

type [<AllowNullLiteral>] RepetitionWithSeparatorStaticOptions =
    abstract definition: ResizeArray<IProduction> with get, set
    abstract separator: TokenType with get, set
    abstract idx: float option with get, set
    abstract name: string option with get, set

/// The Grammar AST class representing a {@link Parser.OR} call.
type [<AllowNullLiteral>] Alternation =
    inherit IProductionWithOccurrence
    inherit IOptionallyNamedProduction
    abstract idx: float with get, set
    abstract name: string with get, set
    abstract definition: ResizeArray<IProduction> with get, set
    abstract accept: visitor: IGASTVisitor -> unit

/// The Grammar AST class representing a {@link Parser.OR} call.
type [<AllowNullLiteral>] AlternationStatic =
    [<Emit "new $0($1...)">] abstract Create: options: AlternationStaticOptions -> Alternation

type [<AllowNullLiteral>] AlternationStaticOptions =
    abstract definition: ResizeArray<IProduction> with get, set
    abstract idx: float option with get, set
    abstract name: string option with get, set

/// The Grammar AST class representing a {@link Parser.CONSUME} call.
type [<AllowNullLiteral>] Terminal =
    inherit IProductionWithOccurrence
    abstract terminalType: TokenType with get, set
    abstract idx: float with get, set
    abstract accept: visitor: IGASTVisitor -> unit

/// The Grammar AST class representing a {@link Parser.CONSUME} call.
type [<AllowNullLiteral>] TerminalStatic =
    [<Emit "new $0($1...)">] abstract Create: options: TerminalStaticOptions -> Terminal

type [<AllowNullLiteral>] TerminalStaticOptions =
    abstract terminalType: TokenType with get, set
    abstract idx: float option with get, set

type [<AllowNullLiteral>] IGASTVisitor =
    abstract visit: prod: IProduction -> obj option

/// Implementing this interface enables customizing grammar validation errors
/// when using custom APIs.
///
/// - See detailed docs for [Custom APIs](http://sap.github.io/chevrotain/docs/guide/custom_apis.html#grammar-validations)
type [<AllowNullLiteral>] IGrammarValidatorErrorMessageProvider =
    abstract buildDuplicateFoundError: topLevelRule: Rule * duplicateProds: ResizeArray<IProductionWithOccurrence> -> string
    abstract buildInvalidNestedRuleNameError: topLevelRule: Rule * nestedProd: IOptionallyNamedProduction -> string
    abstract buildDuplicateNestedRuleNameError: topLevelRule: Rule * nestedProd: ResizeArray<IOptionallyNamedProduction> -> string
    abstract buildNamespaceConflictError: topLevelRule: Rule -> string
    abstract buildAlternationPrefixAmbiguityError: options: IGrammarValidatorErrorMessageProviderBuildAlternationPrefixAmbiguityErrorOptions -> string
    abstract buildAlternationAmbiguityError: options: IGrammarValidatorErrorMessageProviderBuildAlternationAmbiguityErrorOptions -> string
    abstract buildEmptyRepetitionError: options: IGrammarValidatorErrorMessageProviderBuildEmptyRepetitionErrorOptions -> string
    abstract buildTokenNameError: options: IGrammarValidatorErrorMessageProviderBuildTokenNameErrorOptions -> obj option
    abstract buildEmptyAlternationError: options: IGrammarValidatorErrorMessageProviderBuildEmptyAlternationErrorOptions -> obj option
    abstract buildTooManyAlternativesError: options: IGrammarValidatorErrorMessageProviderBuildTooManyAlternativesErrorOptions -> string
    abstract buildLeftRecursionError: options: IGrammarValidatorErrorMessageProviderBuildLeftRecursionErrorOptions -> string
    abstract buildInvalidRuleNameError: options: IGrammarValidatorErrorMessageProviderBuildInvalidRuleNameErrorOptions -> string
    abstract buildDuplicateRuleNameError: options: IGrammarValidatorErrorMessageProviderBuildDuplicateRuleNameErrorOptions -> string

type [<AllowNullLiteral>] IGrammarValidatorErrorMessageProviderBuildAlternationPrefixAmbiguityErrorOptions =
    abstract topLevelRule: Rule with get, set
    abstract prefixPath: ResizeArray<TokenType> with get, set
    abstract ambiguityIndices: ResizeArray<float> with get, set
    abstract alternation: Alternation with get, set

type [<AllowNullLiteral>] IGrammarValidatorErrorMessageProviderBuildAlternationAmbiguityErrorOptions =
    abstract topLevelRule: Rule with get, set
    abstract prefixPath: ResizeArray<TokenType> with get, set
    abstract ambiguityIndices: ResizeArray<float> with get, set
    abstract alternation: Alternation with get, set

type [<AllowNullLiteral>] IGrammarValidatorErrorMessageProviderBuildEmptyRepetitionErrorOptions =
    abstract topLevelRule: Rule with get, set
    abstract repetition: IProductionWithOccurrence with get, set

type [<AllowNullLiteral>] IGrammarValidatorErrorMessageProviderBuildTokenNameErrorOptions =
    abstract tokenType: TokenType with get, set
    abstract expectedPattern: RegExp with get, set

type [<AllowNullLiteral>] IGrammarValidatorErrorMessageProviderBuildEmptyAlternationErrorOptions =
    abstract topLevelRule: Rule with get, set
    abstract alternation: Alternation with get, set
    abstract emptyChoiceIdx: float with get, set

type [<AllowNullLiteral>] IGrammarValidatorErrorMessageProviderBuildTooManyAlternativesErrorOptions =
    abstract topLevelRule: Rule with get, set
    abstract alternation: Alternation with get, set

type [<AllowNullLiteral>] IGrammarValidatorErrorMessageProviderBuildLeftRecursionErrorOptions =
    abstract topLevelRule: Rule with get, set
    abstract leftRecursionPath: ResizeArray<Rule> with get, set

type [<AllowNullLiteral>] IGrammarValidatorErrorMessageProviderBuildInvalidRuleNameErrorOptions =
    abstract topLevelRule: Rule with get, set
    abstract expectedPattern: RegExp with get, set

type [<AllowNullLiteral>] IGrammarValidatorErrorMessageProviderBuildDuplicateRuleNameErrorOptions =
    abstract topLevelRule: U2<Rule, string> with get, set
    abstract grammarName: string with get, set

/// Implementing this interface enables customizing grammar resolving errors
/// when using custom APIs.
///
/// - See detailed docs for [Custom APIs](http://sap.github.io/chevrotain/docs/guide/custom_apis.html#grammar-validations)
type [<AllowNullLiteral>] IGrammarResolverErrorMessageProvider =
    abstract buildRuleNotFoundError: topLevelRule: Rule * undefinedRule: NonTerminal -> string

/// Structure of grammar validations errors.
///
/// - See detailed docs for [Custom APIs](http://sap.github.io/chevrotain/docs/guide/custom_apis.html#grammar-validations)
type [<AllowNullLiteral>] IParserDefinitionError =
    abstract message: string with get, set
    abstract ``type``: ParserDefinitionErrorType with get, set
    abstract ruleName: string option with get, set

/// Structure of configuration object for {@link createSyntaxDiagramsCode}
type [<AllowNullLiteral>] ICreateSyntaxDiagramsConfig =
    /// Base Url to load the runtime resources for rendering the diagrams
    abstract resourceBase: string option with get, set
    /// Url to load the styleSheet, replace with your own for styling customization.
    abstract css: string option with get, set

type [<AllowNullLiteral>] TypeLiteral_01 =
    [<Emit "$0[$1]{{=$2}}">] abstract Item: groupName: string -> ResizeArray<IToken> with get, set

type [<AllowNullLiteral>] TypeLiteral_02 =
    [<Emit "$0[$1]{{=$2}}">] abstract Item: tokType: float -> bool with get, set
