/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package benchmarks;

import com.jcabi.xml.XML;
import fixtures.LargeXmir;
import java.util.concurrent.TimeUnit;
import org.eolang.parser.Xmir;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.OptionsBuilder;

/**
 * Benchmark for XMIR to EO transformations.
 *
 * @since 0.41
 * @checkstyle DesignForExtensionCheck (100 lines)
 * @checkstyle NonStaticMethodCheck (100 lines)
 */
@Fork(1)
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleCorrectTestName"})
public class XmirBench {

    /**
     * Large XMIR document.
     */
    private static final XML XMIR = new LargeXmir("noname", "com/sun/jna/Klass.class").xml();

    /**
     * This main method allows to run the benchmark from IDE.
     * To run benchmarks using Maven, use the command:
     * <p>{@code
     *   mvn jmh:benchmark
     * }</p>
     * @param args Arguments.
     * @throws RunnerException If something goes wrong.
     */
    public static void main(final String[] args) throws RunnerException {
        new Runner(
            new OptionsBuilder()
                .include(XmirBench.class.getSimpleName())
                .build()
        ).run();
    }

    @Benchmark
    public void xmirToEO() {
        new Xmir(XmirBench.XMIR).toEO();
    }
}
