/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package benchmarks;

import java.util.concurrent.TimeUnit;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Fork;
import org.openjdk.jmh.annotations.Level;
import org.openjdk.jmh.annotations.Measurement;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.Warmup;

/**
 * Dataization cost of a literal through the public dataized API.
 *
 * @since 0.1
 * @checkstyle NonStaticMethodCheck (100 lines)
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 8, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Fork(2)
@State(Scope.Thread)
public class LiteralDataizationBench {

    /**
     * A literal reused by the hot benchmark.
     */
    private Phi literal;

    /**
     * New benchmark.
     */
    public LiteralDataizationBench() {
        // JMH constructs benchmark instances.
    }

    /**
     * Prepare one literal for repeated dataization.
     */
    @Setup(Level.Trial)
    public void setUp() {
        this.literal = new Data.ToPhi(42L);
    }

    /**
     * Dataize a newly constructed literal.
     *
     * @return Literal bytes
     */
    @Benchmark
    public byte[] fresh() {
        return new Dataized(new Data.ToPhi(42L)).take();
    }

    /**
     * Dataize one already constructed literal repeatedly.
     *
     * @return Literal bytes
     */
    @Benchmark
    public byte[] hot() {
        return new Dataized(this.literal).take();
    }
}
