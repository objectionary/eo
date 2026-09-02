/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.StreamSupport;
import org.cactoos.Scalar;
import org.cactoos.experimental.Threads;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link CopiedAttrs}.
 * @since 0.63
 */
final class CopiedAttrsTest {

    @Test
    void keepsCopyEmptyAfterLatePutOnOrigin() {
        final Phi origin = new PhDefault(new Attrs(new Attr("v", new AtVoid("v"))));
        final Phi copy = origin.copy();
        origin.put("v", new Data.ToPhi(7L));
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(new PhSafe(copy).take("v")).take(),
            "a copy must not carry a value the origin received after the copy was made"
        );
    }

    @Test
    void readsTheSameCopyWhicheverWayItIsOrdered() {
        final Phi origin = new PhDefault(new Attrs(new Attr("v", new AtVoid("v"))));
        final Phi copy = origin.copy();
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(new PhSafe(copy).take("v")).take(),
            "a copy of an object with an empty attribute must start empty, but it didnt"
        );
        origin.put("v", new Data.ToPhi(7L));
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new Dataized(new PhSafe(copy).take("v")).take(),
            "reading a copy before the origin was filled must not change what the copy holds"
        );
    }

    @Test
    void copiesNoAttributeThatNobodyTakes() {
        final AtomicInteger copies = new AtomicInteger();
        new PhDefault(
            new Attrs(new Attr("x", new CopiedAttrsTest.AtCounting(copies)))
        ).copy();
        MatcherAssert.assertThat(
            "copying an object must leave an attribute nobody takes alone, but it copied it",
            copies.get(),
            Matchers.equalTo(0)
        );
    }

    @Test
    void copiesTheAttributeThatIsTaken() {
        final AtomicInteger copies = new AtomicInteger();
        new PhDefault(
            new Attrs(new Attr("x", new CopiedAttrsTest.AtCounting(copies)))
        ).copy().take("x");
        MatcherAssert.assertThat(
            "taking an attribute out of a copy must copy it once, but it didnt",
            copies.get(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void copiesTheSameAttributeOnlyOnce() {
        final AtomicInteger copies = new AtomicInteger();
        final Phi copy = new PhDefault(
            new Attrs(new Attr("x", new CopiedAttrsTest.AtCounting(copies)))
        ).copy();
        copy.take("x");
        copy.take("x");
        MatcherAssert.assertThat(
            "taking one attribute twice must copy it once, but it copied it again",
            copies.get(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void copiesTheSameAttributeOnlyOnceUnderConcurrentTake() {
        final int threads = 64;
        final AtomicInteger copies = new AtomicInteger();
        final Phi copy = new PhDefault(
            new Attrs(new Attr("x", new CopiedAttrsTest.AtCounting(copies)))
        ).copy();
        MatcherAssert.assertThat(
            "taking one attribute from many threads at once must copy it once, but it copied more",
            new long[] {
                StreamSupport.stream(
                    new Threads<Phi>(
                        threads,
                        IntStream.range(0, threads).mapToObj(
                            idx -> (Scalar<Phi>) () -> copy.take("x")
                        ).collect(Collectors.toList())
                    ).spliterator(), false
                ).count(),
                copies.get(),
            },
            Matchers.equalTo(new long[] {threads, 1})
        );
    }

    @Test
    void losesNoAttributeUnderConcurrentTakeOfDifferentNames() {
        final int threads = 64;
        final Phi copy = new PhDefault(
            new Attrs(
                IntStream.range(0, threads).mapToObj(
                    idx -> new Attr(
                        String.format("a%d", idx), new AtVoid(String.format("a%d", idx))
                    )
                ).toArray(Attr[]::new)
            )
        ).copy();
        MatcherAssert.assertThat(
            "taking every attribute at once must find all of them, but some were lost",
            StreamSupport.stream(
                new Threads<Boolean>(
                    threads,
                    IntStream.range(0, threads).mapToObj(
                        idx -> (Scalar<Boolean>) () -> copy.take(String.format("a%d", idx)) != null
                    ).collect(Collectors.toList())
                ).spliterator(), false
            ).collect(Collectors.toList()),
            Matchers.everyItem(Matchers.is(true))
        );
    }

    @Test
    void doesNotShareOrderWithOriginAfterCopy() {
        final PhDefault origin = new PhDefault();
        origin.add("x", new AtVoid("x"));
        final Phi copy = origin.copy();
        origin.add("y", new AtVoid("y"));
        MatcherAssert.assertThat(
            "a copy must not see an attribute the origin registered later",
            Assertions.assertThrows(
                ExFailure.class,
                () -> copy.put(1, new Data.ToPhi(5.0)),
                "the copy must reject a put past the attribute it had when copied"
            ).getMessage(),
            Matchers.containsString("has just 1 attribute")
        );
    }

    /**
     * Attribute that remembers how many times it was copied.
     * @since 0.63
     */
    private static final class AtCounting implements Attribute {

        /**
         * Where the copies are counted.
         */
        private final AtomicInteger copies;

        /**
         * Ctor.
         * @param count Where to count the copies
         */
        AtCounting(final AtomicInteger count) {
            this.copies = count;
        }

        @Override
        public Attribute copy(final Phi self) {
            this.copies.incrementAndGet();
            return this;
        }

        @Override
        public Phi get() {
            return new PhDefault();
        }

        @Override
        public void put(final Phi phi) {
            throw new UnsupportedOperationException("this attribute takes nothing");
        }

        @Override
        public boolean vacant() {
            return false;
        }

        @Override
        public String φTerm() {
            return "counting";
        }
    }
}
