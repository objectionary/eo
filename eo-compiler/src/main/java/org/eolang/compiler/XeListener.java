/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2020 Yegor Bugayenko
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

import com.jcabi.manifests.Manifests;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
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
@SuppressWarnings({"PMD.TooManyMethods", "PMD.AvoidDuplicateLiterals"})
public final class XeListener implements ProgramListener {

    /**
     * The name of it.
     */
    private final String name;

    /**
     * Xembly directives we are building (mutable).
     */
    private final Directives dirs;

    /**
     * Ctor.
     * @param nme Tha name of it
     */
    public XeListener(final String nme) {
        this.name = nme;
        this.dirs = new Directives();
    }

    /**
     * To get the XML ready to be used.
     * @return XML
     */
    public XML xml() {
        return new XMLDocument(new Xembler(this.dirs).domQuietly());
    }

    @Override
    public void enterProgram(final ProgramParser.ProgramContext ctx) {
        this.dirs.add("program")
            .attr("name", this.name)
            .attr("version", Manifests.read("EO-Version"))
            .attr(
                "time",
                ZonedDateTime.now(ZoneOffset.UTC).format(
                    DateTimeFormatter.ISO_INSTANT
                )
            )
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
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void enterMetas(final ProgramParser.MetasContext ctx) {
        this.dirs.add("metas");
        for (final TerminalNode node : ctx.META()) {
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
        // This method is created by ANTLR and can't be removed
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
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void exitObject(final ProgramParser.ObjectContext ctx) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void enterAbstraction(final ProgramParser.AbstractionContext ctx) {
        this.dirs.add("o").attr("line", ctx.getStart().getLine());
        for (final TerminalNode attr : ctx.NAME()) {
            this.dirs.add("o")
                .attr("line", ctx.getStart().getLine())
                .attr("name", attr.getText())
                .up();
        }
        this.dirs.up();
    }

    @Override
    public void exitAbstraction(final ProgramParser.AbstractionContext ctx) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void enterTail(final ProgramParser.TailContext ctx) {
        this.enter();
    }

    @Override
    public void exitTail(final ProgramParser.TailContext ctx) {
        this.dirs.up();
    }

    @Override
    public void enterSuffix(final ProgramParser.SuffixContext ctx) {
        this.enter();
        this.dirs.attr("name", ctx.name.getText());
        if (ctx.CONST() != null) {
            this.dirs.attr("const", "");
        }
    }

    @Override
    public void exitSuffix(final ProgramParser.SuffixContext ctx) {
        this.dirs.up();
    }

    @Override
    public void enterMethod(final ProgramParser.MethodContext ctx) {
        this.dirs.add("o")
            .attr("line", ctx.getStart().getLine())
            .attr("base", ctx.getText()).up();
    }

    @Override
    public void exitMethod(final ProgramParser.MethodContext ctx) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void enterHead(final ProgramParser.HeadContext ctx) {
        this.dirs.add("o").attr("line", ctx.getStart().getLine());
        if (ctx.NAME() != null) {
            String base = ctx.NAME().getText();
            if (ctx.DOT() != null) {
                base = String.format(".%s", base);
            }
            this.dirs.attr("base", base);
        }
        if (ctx.AT() != null) {
            this.dirs.attr("base", "@");
        }
    }

    @Override
    public void exitHead(final ProgramParser.HeadContext ctx) {
        this.dirs.up();
    }

    @Override
    public void enterHas(final ProgramParser.HasContext ctx) {
        this.enter();
        this.dirs.attr("as", ctx.NAME().getText());
    }

    @Override
    public void exitHas(final ProgramParser.HasContext ctx) {
        this.dirs.up();
    }

    @Override
    public void enterApplication(final ProgramParser.ApplicationContext ctx) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void exitApplication(final ProgramParser.ApplicationContext ctx) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void enterHtail(final ProgramParser.HtailContext ctx) {
        this.enter();
    }

    @Override
    public void exitHtail(final ProgramParser.HtailContext ctx) {
        this.dirs.up();
    }

    // @checkstyle ExecutableStatementCountCheck (100 lines)
    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterData(final ProgramParser.DataContext ctx) {
        final String type;
        final String data;
        if (ctx.BYTES() != null) {
            type = "bytes";
            data = ctx.getText().replace("-", " ").trim();
        } else if (ctx.BOOL() != null) {
            type = "bool";
            data = Boolean.toString(Boolean.parseBoolean(ctx.getText()));
        } else if (ctx.CHAR() != null) {
            type = "char";
            data = ctx.getText().substring(1, 2);
        } else if (ctx.FLOAT() != null) {
            type = "float";
            data = Float.toString(Float.parseFloat(ctx.getText()));
        } else if (ctx.INT() != null) {
            type = "int";
            data = Long.toString(Long.parseLong(ctx.getText()));
        } else if (ctx.HEX() != null) {
            type = "int";
            data = Long.toString(
                // @checkstyle MagicNumberCheck (1 line)
                Long.parseLong(ctx.getText().substring(2), 16)
            );
        } else if (ctx.STRING() != null) {
            type = "string";
            data = ctx.getText().substring(1, ctx.getText().length() - 1);
        } else {
            throw new CompileException("Unknown data type");
        }
        this.dirs.attr("data", type);
        this.dirs.attr("base", type);
        this.dirs.set(data);
    }

    @Override
    public void exitData(final ProgramParser.DataContext ctx) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void visitTerminal(final TerminalNode node) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void visitErrorNode(final ErrorNode node) {
        throw new CompileException(node.getText());
    }

    @Override
    public void enterEveryRule(final ParserRuleContext ctx) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void exitEveryRule(final ParserRuleContext ctx) {
        // This method is created by ANTLR and can't be removed
    }

    /**
     * Help method.
     */
    private void enter() {
        this.dirs.xpath("o[last()]").strict(1);
    }

}
