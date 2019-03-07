import { Lexer, Parser } from "chevrotain"
import Tok from "./Tokens"
import * as I from "./Interop.fs"

class IkigaiParser extends Parser {
    constructor() {
        // we have to explicitly disable the CST building for embedded actions to work.
        super(Tok, { outputCst: false })
        this.performSelfAnalysis()
    }

    public Program = this.RULE("Program", () => {
        let decls: I.Declaration[] = [];
        this.MANY(() => {
            decls.push(this.SUBRULE(this.ValueDeclaration))
        })
        return I.makeProgram(decls)
    })

    // DECLARATIONS ----------------------------------

    public ValueDeclaration = this.RULE("ValueDeclaration", () => {
        this.OPTION(() => {
            this.CONSUME(Tok.ExportModifier)
        })
        const mut = this.CONSUME(Tok.MutabilityModifier).image
        const id = this.CONSUME(Tok.Identifier).image
        this.CONSUME(Tok.Assignment)
        const exp = this.SUBRULE(this.Expression)
        this.CONSUME(Tok.Semicolon)
        return I.makeValueDeclaration(mut, id, exp)
    })

    // EXPRESSION GROUPS ----------------------------------

    public Expression = this.RULE("Expression", () => {
        let expr = this.OR([
            { ALT: () => this.SUBRULE(this.AdditionExpression) },
        ])
        return expr;
    })

    // Addition has lowest precedence thus it is first in the rule chain
    // The precedence of binary expressions is determined by how far down int the Parse Tree appear
    public AdditionExpression = this.RULE("AdditionExpression", () => {
        let items: (I.Expr|string)[] = [];
        items.push(this.SUBRULE(this.ProductExpression))
        this.MANY(() => {
            items.push(this.CONSUME(Tok.AdditionOperator).image)
            items.push(this.SUBRULE2(this.ProductExpression))
        })
        return associateBinaryRight(items)
    })

    public ProductExpression = this.RULE("ProductExpression", () => {
        let items: (I.Expr|string)[] = [];
        items.push(this.SUBRULE(this.ExponentialExpression))
        this.MANY(() => {
            items.push(this.CONSUME(Tok.ProductOperator).image)
            items.push(this.SUBRULE2(this.ExponentialExpression))
        })
        return associateBinaryRight(items)
    })

    public ExponentialExpression = this.RULE("ExponentialExpression", () => {
        let items: (I.Expr|string)[] = [];
        items.push(this.SUBRULE(this.UnaryExpression))
        this.MANY(() => {
            items.push(this.CONSUME(Tok.ExponentialOperator).image)
            items.push(this.SUBRULE2(this.UnaryExpression))
        })
        return associateBinaryRight(items)
    })

    public UnaryExpression = this.RULE("UnaryExpression", () => {
        let op = this.OPTION(() =>
            this.CONSUME(Tok.UnaryOperator).image
        );
        const expr = this.SUBRULE(this.PrimaryExpression)
        return op ? I.makeUnaryOperation(op, expr) : expr;
    })

    public PrimaryExpression = this.RULE("PrimaryExpression", () => {
        return this.OR([
            { ALT: () => this.SUBRULE(this.LambdaExpression) },
            { ALT: () => this.SUBRULE(this.ParensExpression) },
            { ALT: () => this.SUBRULE(this.LiteralExpression) },
            { ALT: () => this.SUBRULE(this.IdentExpression) },
        ])
    })

    public ParensExpression = this.RULE("ParensExpression", () => {
        this.CONSUME(Tok.LParen)
        const expr = this.SUBRULE(this.Expression)
        this.CONSUME(Tok.RParen)
        return expr;
    })

    public LambdaExpression = this.RULE("LambdaExpression", () => {
        const args: I.Argument[] = [];
        this.CONSUME(Tok.LParen)
        this.OPTION(() => {
            args.push(this.SUBRULE(this.Argument))
            this.MANY(() => {
                this.CONSUME(Tok.Comma)
                args.push(this.SUBRULE2(this.Argument))
            })
        })
        this.CONSUME(Tok.RParen)
        this.CONSUME(Tok.Arrow)
        const body = this.OR([
            // { ALT: () => this.SUBRULE(this.Block) },
            { ALT: () => this.SUBRULE(this.Expression) },
        ])
        return I.makeLambdaExpression(args, false, null, body);
    })

    // this.RULE("Block", () => {
    //     this.CONSUME(Tok.LBrace)
    //     // TODO: Statement list
    //     this.CONSUME(Tok.RBrace)
    //     return statements;
    // })

    public Argument = this.RULE("Argument", () => {
        const id = this.CONSUME(Tok.Identifier).image;
        // this.OPTION(() => {
        //     annotation = this.SUBRULE(this.Annotation);
        // });
        // TODO: default value
        return I.makeArgument(id, null, null);
    })

    // this.RULE("Annotation", () => {
    //     this.CONSUME(Tok.Colon)
    //     const id = I.makeIdent(id); // TODO: Generic args, ad-hoc signatures
    //     return I.makeAnnotation(id);
    // })

    public IdentExpression = this.RULE("IdentExpression", () => {
        const id = this.CONSUME(Tok.Identifier).image;
        return I.makeIdent(id)
    })

    // LITERALS --------------------------------------

    // TODO: Booleans, null...
    public LiteralExpression = this.RULE("LiteralExpression", () => {
        let tup = this.OR([
            { ALT: () => ["number", this.CONSUME(Tok.Number).image] },
            { ALT: () => ["string", this.CONSUME(Tok.String).image] }
        ])
        return I.makeLiteral(tup[0], tup[1]);
    })
}

// Items must be in the format: [expr, op, expr, op, expr...]
function associateBinaryRight(items: (I.Expr|string)[]): I.Expr {
    // TODO: Throw error if length is 0;
    if (items.length === 1) {
        return items[0] as I.Expr;
    } else {
        let lastExpr = items[items.length - 1];
        for (let i = items.length - 2; i > 0; i -= 2) {
            lastExpr = I.makeBinaryOperation(items[i - 1], items[i] as string, lastExpr);
        }
        return lastExpr as I.Expr;
    }
}

const parser = new IkigaiParser()
const lexer = new Lexer(Object.keys(Tok).map(k => Tok[k]));

export function parse(text) {
    const lexingResult = lexer.tokenize(text)
    // "input" is a setter which will reset the parser's state.
    parser.input = lexingResult.tokens
    const ast = parser.Program()
    return {
        ast,
        errors: parser.errors
    }
}
