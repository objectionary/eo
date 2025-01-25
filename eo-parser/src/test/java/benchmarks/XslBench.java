/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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
package benchmarks;

import com.jcabi.xml.XML;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import fixtures.LargeXmir;
import java.util.concurrent.TimeUnit;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;

/**
 * Benchmark for XSL transformations.
 *
 * @since 0.41
 */
@Fork(1)
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleCorrectTestName"})
public class XslBench {

    /**
     * Large XMIR document.
     */
    private final XML xmir = new LargeXmir().xml();

    /**
     * All sheets to use.
     */
    private final Train<Shift> sheets = new TrClasspath<>(
        "/org/eolang/parser/parse/move-voids-up.xsl",
        "/org/eolang/parser/parse/validate-before-stars.xsl",
        "/org/eolang/parser/parse/resolve-before-star.xsl",
        "/org/eolang/parser/parse/wrap-method-calls.xsl",
        "/org/eolang/parser/parse/const-to-dataized.xsl",
        "/org/eolang/parser/parse/stars-to-tuples.xsl",
        "/org/eolang/parser/shake/add-default-package.xsl",
        "/org/eolang/parser/shake/build-fqns.xsl",
        "/org/eolang/parser/shake/explicit-data.xsl"
    ).back();

    @Benchmark
    public final void manySheetsOnLargeXmir() {
        new Xsline(this.sheets).pass(this.xmir);
    }
}
