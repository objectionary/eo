/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
package org.eolang.parser;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;

/**
 * The phi-calculus grammar listener for ANTLR4 walker.
 *
 * @checkstyle CyclomaticComplexityCheck (500 lines)
 * @checkstyle ClassFanOutComplexityCheck (500 lines)
 * @checkstyle MethodCountCheck (1300 lines)
 * @since 0.34.0
 */
@SuppressWarnings({
    "PMD.TooManyMethods",
    "PMD.AvoidDuplicateLiterals",
    "PMD.ExcessivePublicCount",
    "PMD.ExcessiveClassLength"
})
public final class XePhiListener implements PhiListener {
    @Override
    public void enterProgram(PhiParser.ProgramContext ctx) {

    }

    @Override
    public void exitProgram(PhiParser.ProgramContext ctx) {

    }

    @Override
    public void enterObject(PhiParser.ObjectContext ctx) {

    }

    @Override
    public void exitObject(PhiParser.ObjectContext ctx) {

    }

    @Override
    public void enterFormation(PhiParser.FormationContext ctx) {

    }

    @Override
    public void exitFormation(PhiParser.FormationContext ctx) {

    }

    @Override
    public void enterBindings(PhiParser.BindingsContext ctx) {

    }

    @Override
    public void exitBindings(PhiParser.BindingsContext ctx) {

    }

    @Override
    public void enterBinding(PhiParser.BindingContext ctx) {

    }

    @Override
    public void exitBinding(PhiParser.BindingContext ctx) {

    }

    @Override
    public void enterAlphaBinding(PhiParser.AlphaBindingContext ctx) {

    }

    @Override
    public void exitAlphaBinding(PhiParser.AlphaBindingContext ctx) {

    }

    @Override
    public void enterAttribute(PhiParser.AttributeContext ctx) {

    }

    @Override
    public void exitAttribute(PhiParser.AttributeContext ctx) {

    }

    @Override
    public void enterAlpha(PhiParser.AlphaContext ctx) {

    }

    @Override
    public void exitAlpha(PhiParser.AlphaContext ctx) {

    }

    @Override
    public void enterEmptyBinding(PhiParser.EmptyBindingContext ctx) {

    }

    @Override
    public void exitEmptyBinding(PhiParser.EmptyBindingContext ctx) {

    }

    @Override
    public void enterDeltaBidning(PhiParser.DeltaBidningContext ctx) {

    }

    @Override
    public void exitDeltaBidning(PhiParser.DeltaBidningContext ctx) {

    }

    @Override
    public void enterLambdaBidning(PhiParser.LambdaBidningContext ctx) {

    }

    @Override
    public void exitLambdaBidning(PhiParser.LambdaBidningContext ctx) {

    }

    @Override
    public void enterApplication(PhiParser.ApplicationContext ctx) {

    }

    @Override
    public void exitApplication(PhiParser.ApplicationContext ctx) {

    }

    @Override
    public void enterBnds(PhiParser.BndsContext ctx) {

    }

    @Override
    public void exitBnds(PhiParser.BndsContext ctx) {

    }

    @Override
    public void enterDispatch(PhiParser.DispatchContext ctx) {

    }

    @Override
    public void exitDispatch(PhiParser.DispatchContext ctx) {

    }

    @Override
    public void enterAttrs(PhiParser.AttrsContext ctx) {

    }

    @Override
    public void exitAttrs(PhiParser.AttrsContext ctx) {

    }

    @Override
    public void enterTermination(PhiParser.TerminationContext ctx) {

    }

    @Override
    public void exitTermination(PhiParser.TerminationContext ctx) {

    }

    @Override
    public void visitTerminal(TerminalNode terminalNode) {

    }

    @Override
    public void visitErrorNode(ErrorNode errorNode) {

    }

    @Override
    public void enterEveryRule(ParserRuleContext parserRuleContext) {

    }

    @Override
    public void exitEveryRule(ParserRuleContext parserRuleContext) {

    }
}