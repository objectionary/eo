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
        this.dirs.add("program");
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
                .add("head").set(parts[0].substring(1)).up()
                .add("tail").set(parts[1].trim()).up()
                .up();
        }
        this.dirs.up();
    }

    @Override
    public void exitMetas(final ProgramParser.MetasContext ctx) {
    }

    @Override
    public void enterObject(final ProgramParser.ObjectContext ctx) {
    }

    @Override
    public void exitObject(final ProgramParser.ObjectContext ctx) {
    }

    @Override
    public void enterVobject(final ProgramParser.VobjectContext ctx) {
    }

    @Override
    public void exitVobject(final ProgramParser.VobjectContext ctx) {
    }

    @Override
    public void enterVhead(final ProgramParser.VheadContext ctx) {
        this.dirs.add("o");
        if (ctx.NAME() != null) {
            this.dirs.attr("base", ctx.NAME().getText());
        }
    }

    @Override
    public void exitVhead(final ProgramParser.VheadContext ctx) {
        this.dirs.up();
    }

    @Override
    public void enterVtail(final ProgramParser.VtailContext ctx) {
        this.dirs.xpath("o[last()]").strict(1);
    }

    @Override
    public void exitVtail(final ProgramParser.VtailContext ctx) {
        this.dirs.up();
    }

    @Override
    public void enterAttributes(final ProgramParser.AttributesContext ctx) {
        for (TerminalNode attr : ctx.NAME()) {
            this.dirs.add("o")
                .attr("name", attr.getText())
                .up();
        }
    }

    @Override
    public void exitAttributes(final ProgramParser.AttributesContext ctx) {
    }

    @Override
    public void enterSuffix(final ProgramParser.SuffixContext ctx) {
        if (ctx.NAME() != null) {
            this.dirs.attr("name", ctx.NAME().getText());
        }
    }

    @Override
    public void exitSuffix(final ProgramParser.SuffixContext ctx) {
    }

    @Override
    public void enterMethod(final ProgramParser.MethodContext ctx) {
        this.dirs
            .xpath("o[last()]").strict(1).attr("name", "").up()
            .add("o")
            .attr("base", ctx.getText()).up();
    }

    @Override
    public void exitMethod(final ProgramParser.MethodContext ctx) {
    }

    @Override
    public void enterHhead(final ProgramParser.HheadContext ctx) {
        this.dirs.add("o");
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
    public void enterHobject(final ProgramParser.HobjectContext ctx) {
    }

    @Override
    public void exitHobject(final ProgramParser.HobjectContext ctx) {
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
    public void enterHtail(final ProgramParser.HtailContext ctx) {
        this.dirs.xpath("o[last()]").strict(1);
    }

    @Override
    public void exitHtail(final ProgramParser.HtailContext ctx) {
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
        this.dirs.attr("atom", type);
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
