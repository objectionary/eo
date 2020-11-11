/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016 eolang.org
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.compiler;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.cactoos.list.Mapped;
import org.cactoos.text.Joined;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Program.
 *
 * @since 0.1
 */
public final class XeListener implements ProgramListener {

    private final Directives dirs = new Directives();

    public String xml() {
        return new Xembler(this.dirs).xmlQuietly();
    }

    @Override
    public void enterProgram(final ProgramParser.ProgramContext ctx) {
        this.dirs.add("program")
            .add("errors").up();
    }

    @Override
    public void exitProgram(final ProgramParser.ProgramContext ctx) {
        this.dirs.up();
    }

    @Override
    public void enterLicense(final ProgramParser.LicenseContext ctx) {
        this.dirs.add("license").set(
            new Joined(
                "\n",
                new Mapped<>(
                    cmt -> cmt.getText().substring(1).trim(),
                    ctx.COMMENT()
                )
            )
        ).up();
    }

    @Override
    public void exitLicense(final ProgramParser.LicenseContext ctx) {
    }

    @Override
    public void enterMetas(final ProgramParser.MetasContext ctx) {
        this.dirs.add("metas");
        for (TerminalNode node : ctx.META()) {
            final String[] parts = node.getText().split(" ", 2);
            this.dirs.add("meta")
                .attr("line", node.getSymbol().getLine())
                .add("head").set(parts[0].substring(1)).up()
                .add("tail");
            if (parts.length > 1) {
                this.dirs.set(parts[1].trim());
            }
            this.dirs.up().up();
        }
        this.dirs.up();
    }

    @Override
    public void exitMetas(final ProgramParser.MetasContext ctx) {
    }

    @Override
    public void enterObjects(final ProgramParser.ObjectsContext ctx) {
        this.dirs.add("objects");
    }

    @Override
    public void exitObjects(final ProgramParser.ObjectsContext ctx) {
        this.dirs.up();
    }

    @Override
    public void enterObject(final ProgramParser.ObjectContext ctx) {
    }

    @Override
    public void exitObject(final ProgramParser.ObjectContext ctx) {
    }

    @Override
    public void enterAbstraction(final ProgramParser.AbstractionContext ctx) {
        this.dirs.add("o").attr("line", ctx.getStart().getLine());
    }

    @Override
    public void exitAbstraction(final ProgramParser.AbstractionContext ctx) {
        this.dirs.up();
    }

    @Override
    public void enterTail(final ProgramParser.TailContext ctx) {
        this.dirs.xpath("o[last()]").strict(1);
    }

    @Override
    public void exitTail(final ProgramParser.TailContext ctx) {
        this.dirs.up();
    }

    @Override
    public void enterAttributes(final ProgramParser.AttributesContext ctx) {
        for (TerminalNode attr : ctx.NAME()) {
            this.dirs.add("o")
                .attr("line", ctx.getStart().getLine())
                .attr("name", attr.getText())
                .up();
        }
    }

    @Override
    public void exitAttributes(final ProgramParser.AttributesContext ctx) {
    }

    @Override
    public void enterSuffix(final ProgramParser.SuffixContext ctx) {
        this.dirs.attr("name", ctx.NAME().getText());
        if (ctx.CONST() != null) {
            this.dirs.attr("const", "");
        }
    }

    @Override
    public void exitSuffix(final ProgramParser.SuffixContext ctx) {
    }

    @Override
    public void enterMethod(final ProgramParser.MethodContext ctx) {
        this.dirs
            .xpath("o[last()]").strict(1).up()
            .add("o")
            .attr("line", ctx.getStart().getLine())
            .attr("base", ctx.getText()).up();
    }

    @Override
    public void exitMethod(final ProgramParser.MethodContext ctx) {
    }

    @Override
    public void enterHhead(final ProgramParser.HheadContext ctx) {
        this.dirs.add("o").attr("line", ctx.getStart().getLine());
        if (ctx.NAME() != null) {
            this.dirs.attr("base", ctx.NAME().getText());
        }
        if (ctx.AT() != null) {
            this.dirs.attr("base", "@");
        }
    }

    @Override
    public void exitHhead(final ProgramParser.HheadContext ctx) {
        this.dirs.up();
    }

    @Override
    public void enterHas(final ProgramParser.HasContext ctx) {
        this.dirs.xpath("o[last()]").strict(1).attr("as", ctx.NAME().getText());
    }

    @Override
    public void exitHas(final ProgramParser.HasContext ctx) {
        this.dirs.up();
    }

    @Override
    public void enterApplication(final ProgramParser.ApplicationContext ctx) {
    }

    @Override
    public void exitApplication(final ProgramParser.ApplicationContext ctx) {
    }

    @Override
    public void enterHtail(final ProgramParser.HtailContext ctx) {
        this.dirs.xpath("o[last()]").strict(1);
    }

    @Override
    public void exitHtail(final ProgramParser.HtailContext ctx) {
        this.dirs.up();
    }

    @Override
    public void enterHsuffix(final ProgramParser.HsuffixContext ctx) {
        this.dirs.xpath("o[last()]").strict(1);
    }

    @Override
    public void exitHsuffix(final ProgramParser.HsuffixContext ctx) {
        this.dirs.up();
    }

    @Override
    public void enterData(final ProgramParser.DataContext ctx) {
        final String type;
        final String data;
        if (ctx.CHAR() != null) {
            type = "char";
            data = ctx.getText().substring(1, 1);
        } else if (ctx.FLOAT() != null) {
            type = "float";
            data = Float.toString(Float.parseFloat(ctx.getText()));
        } else if (ctx.INTEGER() != null) {
            type = "float";
            data = Integer.toString(Integer.parseInt(ctx.getText()));
        } else if (ctx.FLOAT() != null) {
            type = "hex";
            data = ctx.getText();
        } else if (ctx.STRING() != null) {
            type = "string";
            data = ctx.getText().substring(1, ctx.getText().length() - 1);
        } else {
            throw new CompileException("Unknown data type");
        }
        this.dirs.attr("base", type);
        this.dirs.set(data);
    }

    @Override
    public void exitData(final ProgramParser.DataContext ctx) {
    }

    @Override
    public void visitTerminal(final TerminalNode terminalNode) {
    }

    @Override
    public void visitErrorNode(final ErrorNode errorNode) {
        throw new CompileException(errorNode.getText());
    }

    @Override
    public void enterEveryRule(final ParserRuleContext parserRuleContext) {
    }

    @Override
    public void exitEveryRule(final ParserRuleContext parserRuleContext) {
    }
}
