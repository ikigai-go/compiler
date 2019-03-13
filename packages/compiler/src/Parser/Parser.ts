import { Lexer, Parser, IToken } from "chevrotain"
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
            decls.push(this.SUBRULE(this.Declaration))
        })
        return I.makeProgram(decls)
    })

    // DECLARATIONS ----------------------------------

    public Declaration = this.RULE("Declaration", () => {
        const exportToken = this.OPTION(() => {
            this.CONSUME(Tok.ExportModifier)
        })
        const decl = this.OR([
            { ALT: () => this.SUBRULE(this.SkillDeclaration) },
            { ALT: () => this.SUBRULE(this.TrainDeclaration) },
            { ALT: () => this.SUBRULE(this.ValueDeclaration) },
        ]);
        return I.makeDeclaration(exportToken != null, decl);
    })

    public SkillDeclaration = this.RULE("SkillDeclaration", () => {
        const signatures: I.Signature[] = [];
        this.CONSUME(Tok.Skill)
        const name = this.CONSUME(Tok.Identifier);
        this.CONSUME(Tok.LAngleBracket);
        // TODO: Stricter rules for generic params?
        // (e.g. single uppercase letter with optional digit)
        const genericParam = this.CONSUME2(Tok.Identifier);
        this.CONSUME(Tok.RAngleBracket);
        this.CONSUME(Tok.LBrace);
        this.MANY(() => {
            signatures.push(this.SUBRULE2(this.Signature))
        })
        this.CONSUME(Tok.RBrace);
        return I.makeSkillDeclaration(name, genericParam, signatures);
    })

    // TODO: Other non-method signatures
    public Signature = this.RULE("Signature", () => {
        const name = this.CONSUME(Tok.Identifier);
        const args: I.ArgumentSignature[] = [];
        this.CONSUME(Tok.LParen)
        this.OPTION(() => {
            args.push(this.SUBRULE(this.ArgumentSignature))
            this.MANY(() => {
                this.CONSUME(Tok.Comma)
                args.push(this.SUBRULE2(this.ArgumentSignature))
            })
        })
        this.CONSUME(Tok.RParen)
        this.CONSUME(Tok.Colon);
        const returnType = this.SUBRULE(this.Type);
        this.CONSUME(Tok.Semicolon);
        // TODO: hasSpread
        return I.makeMethodSignature(name, args, false, returnType);
    })

    public TrainDeclaration = this.RULE("TrainDeclaration", () => {
        debugger;
        const members: I.Member[] = [];
        this.CONSUME(Tok.Train)
        const skillName = this.CONSUME(Tok.Identifier);
        this.CONSUME(Tok.LAngleBracket);
        const trainedType = this.SUBRULE(this.Type);
        this.CONSUME(Tok.RAngleBracket);
        this.CONSUME(Tok.LBrace);
        this.MANY(() => {
            members.push(this.SUBRULE2(this.Member))
        })
        this.CONSUME(Tok.RBrace);
        return I.makeTrainDeclaration(skillName, trainedType, members);
    })

    // TODO: Other non-method members
    public Member = this.RULE("Member", () => {
        const name = this.CONSUME(Tok.Identifier);
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
        const returnType = this.OPTION2(() => {
            this.CONSUME(Tok.Colon);
            return this.SUBRULE(this.Type);
        });
        const body = this.OR([
            // { ALT: () => this.SUBRULE(this.Block) },
            { ALT: () => {
                this.CONSUME(Tok.Arrow)
                return this.SUBRULE(this.Expression)
            } }
        ]);
        this.CONSUME(Tok.Semicolon);
        // TODO: hasSpread
        return I.makeMethod(name, args, false, returnType, body);
    })

    public ValueDeclaration = this.RULE("ValueDeclaration", () => {
        const mut = this.CONSUME(Tok.MutabilityModifier).image
        const id = this.CONSUME(Tok.Identifier)
        this.CONSUME(Tok.Assignment)
        const exp = this.SUBRULE(this.Expression)
        this.CONSUME(Tok.Semicolon)
        return I.makeValueDeclaration(mut, id, exp)
    })

    // EXPRESSION GROUPS ----------------------------------

    public Expression = this.RULE("Expression", () => {
        return this.OR([
            { ALT: () => this.SUBRULE(this.AdditionExpression) },
        ])
    })

    // Addition has lowest precedence thus it is first in the rule chain
    // The precedence of binary expressions is determined by how far down int the Parse Tree appear
    public AdditionExpression = this.RULE("AdditionExpression", () => {
        let items: (I.Expr|IToken)[] = [];
        items.push(this.SUBRULE(this.ProductExpression))
        this.MANY(() => {
            items.push(this.CONSUME(Tok.AdditionOperator))
            items.push(this.SUBRULE2(this.ProductExpression))
        })
        return associateBinaryRight(items)
    })

    public ProductExpression = this.RULE("ProductExpression", () => {
        let items: (I.Expr|IToken)[] = [];
        items.push(this.SUBRULE(this.ExponentialExpression))
        this.MANY(() => {
            items.push(this.CONSUME(Tok.ProductOperator))
            items.push(this.SUBRULE2(this.ExponentialExpression))
        })
        return associateBinaryRight(items)
    })

    public ExponentialExpression = this.RULE("ExponentialExpression", () => {
        let items: (I.Expr|IToken)[] = [];
        items.push(this.SUBRULE(this.UnaryExpression))
        this.MANY(() => {
            items.push(this.CONSUME(Tok.ExponentialOperator))
            items.push(this.SUBRULE2(this.UnaryExpression))
        })
        return associateBinaryRight(items)
    })

    public UnaryExpression = this.RULE("UnaryExpression", () => {
        let op = this.OPTION(() =>
            this.CONSUME(Tok.UnaryOperator));
        const expr = this.SUBRULE(this.PrimaryExpression)
        return op ? I.makeUnaryOperation(op, expr) : expr;
    })

    public PrimaryExpression = this.RULE("PrimaryExpression", () => {
        return this.OR([
            { ALT: () => this.SUBRULE(this.LambdaExpression) },
            { ALT: () => this.SUBRULE(this.CallExpression) },
        ])
    })

    public CallExpression = this.RULE("CallExpression", () => {
        const newTok = this.OPTION(() => this.CONSUME(Tok.NewTok));
        const baseExpr = this.SUBRULE(this.MemberExpression);
        const callOp = this.OPTION2(() => {
            const argExprs: I.Expr[] = [];
            this.CONSUME(Tok.LParen)
            this.OPTION3(() => {
                argExprs.push(this.SUBRULE(this.Expression))
                this.MANY(() => {
                    this.CONSUME(Tok.Comma)
                    argExprs.push(this.SUBRULE2(this.Expression))
                })
            })
            const lastParen = this.CONSUME(Tok.RParen)
            const range1 = newTok != null ? I.rangeFromToken(newTok) : I.rangeFromExpr(baseExpr);
            const range = I.addRanges(range1, I.rangeFromToken(lastParen));
            // TODO: hasSpread
            return I.makeCallOperation(baseExpr, argExprs, newTok != null, false, range);
        })

        return callOp || baseExpr;
    })

    public MemberExpression = this.RULE("MemberExpression", () => {
        const baseExpr = this.OR([
            { ALT: () => this.SUBRULE(this.ParensExpression) },
            { ALT: () => this.SUBRULE(this.LiteralExpression) },
            { ALT: () => I.makeIdent(this.CONSUME(Tok.Identifier)) },
        ])
        const memberExpr = this.OPTION(() => this.OR2([
            { ALT: () => {
                this.CONSUME(Tok.LBracket)
                const e = this.SUBRULE(this.Expression)
                this.CONSUME(Tok.RBracket)
                return e;
            } },
            { ALT: () => {
                this.CONSUME(Tok.Dot)
                return I.makeLiteral("string", this.CONSUME2(Tok.Identifier))
            } },
        ]));
        return memberExpr == null
            ? baseExpr
            : I.makeGetExpression(baseExpr, memberExpr);
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
        const id = this.CONSUME(Tok.Identifier);
        const annotation = this.OPTION(() => {
            this.CONSUME(Tok.Colon)
            return this.SUBRULE(this.Type)
        });
        // TODO: default value
        return I.makeArgument(id, annotation, null);
    })

    public ArgumentSignature = this.RULE("ArgumentSignature", () => {
        const id = this.CONSUME(Tok.Identifier);
        this.CONSUME(Tok.Colon);
        const annotation = this.SUBRULE(this.Type);
        // TODO: isOptional
        return I.makeArgumentSignature(id, false, annotation);
    })

    public Type = this.RULE("Type", () => {
        // TODO: ad-hoc signatures
        const id = this.CONSUME(Tok.Identifier);
        const genArgs: I.Type[] = []
        this.OPTION(() => {
            this.CONSUME(Tok.LAngleBracket);
            genArgs.push(this.SUBRULE(this.Type));
            this.MANY(() => {
                this.CONSUME(Tok.Comma);
                genArgs.push(this.SUBRULE2(this.Type));
            })
            this.CONSUME(Tok.RAngleBracket);
        });
        return I.makeType(id, genArgs);
    })

    // LITERALS --------------------------------------

    // TODO: Booleans, null...
    public LiteralExpression = this.RULE("LiteralExpression", () => {
        return this.OR([
            { ALT: () => I.makeLiteral("number", this.CONSUME(Tok.Number)) },
            { ALT: () => I.makeLiteral("string", this.CONSUME(Tok.String)) },
            { ALT: () => I.makeLiteral("boolean", this.CONSUME(Tok.Bool)) },
            { ALT: () => I.makeLiteral("null", this.CONSUME(Tok.Null)) },
        ]);
    })
}

// Items must be in the format: [expr, op, expr, op, expr...]
function associateBinaryRight(items: (I.Expr|IToken)[]): I.Expr {
    // TODO: Throw error if length is 0;
    if (items.length === 1) {
        return items[0] as I.Expr;
    } else {
        let lastExpr = items[items.length - 1];
        for (let i = items.length - 2; i > 0; i -= 2) {
            lastExpr = I.makeBinaryOperation(items[i - 1] as I.Expr, items[i] as IToken, lastExpr as I.Expr);
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
