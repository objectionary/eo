/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package benchmarks;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.util.concurrent.TimeUnit;
import org.eolang.printer.Xmir;
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
 * Benchmark for printing a deeply nested chain of reversed dispatches
 * ({@code 5.plus 1 .plus 2 ...}), the shape that repeatedly exercises
 * {@code Pretty.suffixed}/{@code Pretty.flat} at every nesting level.
 * @since 0.1
 * @checkstyle NonStaticMethodCheck (100 lines)
 */
@Fork(1)
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
@SuppressWarnings({
    "JTCOP.RuleAllTestsHaveProductionClass",
    "JTCOP.RuleCorrectTestName",
    "PMD.ConstructorShouldDoInitialization"
})
public class PrettyBench {

    /**
     * A deeply nested chain of reversed dispatches.
     */
    private final XML input = new XMLDocument(
        String.format(
            "<object><metas/><o name='main'>%s</o></object>",
            this.chain(24)
        )
    );

    /**
     * Ctor.
     */
    public PrettyBench() {
        // nothing
    }

    /**
     * Print the chain to EO.
     */
    @Benchmark
    public final void printsNestedReversedDispatches() {
        new Xmir(this.input).toEO();
    }

    private String chain(final int depth) {
        String xml = "<o base='Φ.number'>0</o>";
        for (int lvl = 0; lvl < depth; lvl = lvl + 1) {
            xml = String.format(
                "<o base='.plus'>%s<o base='Φ.number'>%d</o></o>", xml, lvl
            );
        }
        return xml;
    }
}
