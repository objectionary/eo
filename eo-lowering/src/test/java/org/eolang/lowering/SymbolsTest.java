/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Symbols}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class SymbolsTest {

    @Test
    void mintsFirstSymbolAsRow(@Mktmp final Path temp) throws IOException {
        final Path table = temp.resolve("symbols.tsv");
        new Symbols(table).minted("number", Arrays.asList("void", "a"));
        MatcherAssert.assertThat(
            "the first symbol must land as a tab separated row, but it didnt",
            new String(Files.readAllBytes(table), StandardCharsets.UTF_8),
            Matchers.equalTo("S1\tnumber\tvoid\ta\n")
        );
    }

    @Test
    void reusesSymbolOfIdenticalCells(@Mktmp final Path temp) throws IOException {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        table.minted("number", Arrays.asList("void", "a"));
        table.minted("number", Arrays.asList("L_number_plus", "sym:S1", "sym:S1"));
        MatcherAssert.assertThat(
            "the same cells must answer the same symbol, but they didnt",
            table.minted("number", Arrays.asList("L_number_plus", "sym:S1", "sym:S1")),
            Matchers.equalTo("S2")
        );
    }

    @Test
    void mintsFreshSymbolDespiteIdenticalCells(@Mktmp final Path temp) throws IOException {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        table.fresh("object", Arrays.asList("fork", "sym:S1"));
        MatcherAssert.assertThat(
            "a fresh symbol cannot be shared with an earlier row, but it was",
            table.fresh("object", Arrays.asList("fork", "sym:S1")),
            Matchers.equalTo("S2")
        );
    }

    @Test
    void countsDistinctSymbolsOnly(@Mktmp final Path temp) throws IOException {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        table.fresh("object", Arrays.asList("fork", "sym:S1"));
        table.record("S1", "left");
        table.record("S1", "end");
        MatcherAssert.assertThat(
            "the rows of one symbol cannot advance the counter, but they did",
            table.fresh("number", Arrays.asList("void", "x")),
            Matchers.equalTo("S2")
        );
    }

    @Test
    void retypesCarrierOfSymbol(@Mktmp final Path temp) throws IOException {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        table.fresh("object", Arrays.asList("fork", "sym:S3"));
        table.retyped("S1", "bool");
        MatcherAssert.assertThat(
            "the carrier of the retyped row must change, but it didnt",
            table.carrier("S1"),
            Matchers.equalTo("bool")
        );
    }

    @Test
    void readsRowOfSymbol(@Mktmp final Path temp) throws IOException {
        final Symbols table = new Symbols(temp.resolve("s.tsv"));
        table.fresh("bytes", Arrays.asList("box", "Φ.foo.bar", "x=number:01-"));
        MatcherAssert.assertThat(
            "the row must come back with all its cells, but it didnt",
            table.row("S1"),
            Matchers.contains("S1", "bytes", "box", "Φ.foo.bar", "x=number:01-")
        );
    }

    @Test
    void refusesUnknownSymbol(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Symbols(temp.resolve("s.tsv")).row("S8"),
            "a symbol without a row must be refused, but it wasnt"
        );
    }
}
