/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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
package org.eolang.maven.optimization;

import com.jcabi.xml.XML;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrFast;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;

/**
 * Optimisation train of XLS`s.
 * @since 0.28.12
 */
public final class OptTrain implements Optimization {

    /**
     * Parsing train with XSLs.
     *
     * @implNote The list of applied XSLs is adjusted during execution.
     * <br>Separate instance of the train is used of each optimization
     * thread since {@link com.jcabi.xml.XSLDocument}, which is used under
     * the hood in {@link TrClasspath}, is not thread-safe.
     */
    static final Train<Shift> DEFAULT_TRAIN = new TrFast(
        new TrClasspath<>(
            new TrDefault<>(),
            "/org/eolang/parser/optimize/globals-to-abstracts.xsl",
            "/org/eolang/parser/optimize/remove-refs.xsl",
            "/org/eolang/parser/optimize/abstracts-float-up.xsl",
            "/org/eolang/parser/optimize/remove-levels.xsl",
            "/org/eolang/parser/add-refs.xsl",
            "/org/eolang/parser/optimize/fix-missed-names.xsl",
            "/org/eolang/parser/add-refs.xsl",
            "/org/eolang/parser/errors/broken-refs.xsl",
            "/org/eolang/parser/optimize/constant-folding.xsl",
            "/org/eolang/parser/set-locators.xsl"
        ).back(),
        TrFast.class,
        500L
    );

    /**
     * Delegate.
     */
    private final Optimization delegate;

    /**
     * Shifts that we are going to apply.
     */
    private final Train<Shift> shifts;

    /**
     * The default constructor with the default preset of xsl optimizations.
     */
    public OptTrain() {
        this(OptTrain.DEFAULT_TRAIN);
    }

    /**
     * Constructor that accepts train of shifts.
     *
     * @param shifts XLS shifts.
     */
    public OptTrain(final Train<Shift> shifts) {
        this(xml -> xml, shifts);
    }

    /**
     * Constructor for single {@link StClasspath} optimization.
     *
     * @param delegate Optimizations that have to be done before.
     * @param xls File from classpath.
     */
    public OptTrain(final Optimization delegate, final String xls) {
        this(
            delegate,
            new TrDefault<Shift>().with(new StClasspath(xls))
        );
    }

    /**
     * The default constructor.
     * @param delegate Optimizations that have to be done before.
     * @param shifts To apply
     */
    public OptTrain(
        final Optimization delegate,
        final Train<Shift> shifts
    ) {
        this.delegate = delegate;
        this.shifts = shifts;
    }

    @Override
    public XML apply(final XML xml) {
        return new Xsline(this.shifts).pass(this.delegate.apply(xml));
    }
}
