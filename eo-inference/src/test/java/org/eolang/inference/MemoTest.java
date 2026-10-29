/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicInteger;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Memo}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class MemoTest {

    @Test
    void worksOutQuestionOncePerPass(@Mktmp final Path temp) throws IOException {
        final Memo memo = new State(new Sources(temp).program(), new Names()).memo();
        final AtomicInteger asked = new AtomicInteger();
        final Key key = new Key('k', 31, "", "", "", 0);
        memo.ask(key, () -> new Ints(asked.incrementAndGet()));
        memo.ask(key, () -> new Ints(asked.incrementAndGet()));
        MatcherAssert.assertThat(
            "a question asked twice in a pass must be worked out once, but it wasnt",
            asked.get(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void joinsNewAnswerToStoredOne(@Mktmp final Path temp) throws IOException {
        final Memo memo = new State(new Sources(temp).program(), new Names()).memo();
        final Key key = new Key('e', 7, "", "Φ.vase.φ", "", 0);
        memo.ask(key, () -> new Ints(Values.DATA));
        memo.stale(key);
        memo.invalidate();
        MatcherAssert.assertThat(
            "an answer worked out again must keep what it had, but it didnt",
            memo.ask(key, () -> new Ints(Values.TERM)),
            Matchers.equalTo(new Ids(Values.DATA, Values.TERM))
        );
    }

    @Test
    void givesWhatItHasSoFarToQuestionMetAgain(@Mktmp final Path temp) throws IOException {
        final Memo memo = new State(new Sources(temp).program(), new Names()).memo();
        final Key key = new Key('a', 12, "next", "Φ.chain.φ", "", 0);
        memo.ask(
            key,
            () -> {
                final Ints inner = new Ints(Values.ROOT);
                inner.addAll(memo.ask(key, () -> new Ints(Values.TERM)));
                return inner;
            }
        );
        MatcherAssert.assertThat(
            "a question met again while it is asked must be a cycle hit, but it wasnt",
            memo.hits(),
            Matchers.equalTo(1L)
        );
    }

    @Test
    void losesAnswerWhenWhatItReadGrows(@Mktmp final Path temp) throws IOException {
        final Memo memo = new State(new Sources(temp).program(), new Names()).memo();
        final AtomicInteger asked = new AtomicInteger();
        final Key key = new Key('k', 4, "", "", "", 0);
        final Key row = new Key('r', 9, "lid", "", "", 0);
        memo.ask(
            key,
            () -> {
                memo.depend(row);
                return new Ints(asked.incrementAndGet());
            }
        );
        memo.spoil(row);
        memo.invalidate();
        memo.ask(key, () -> new Ints(asked.incrementAndGet()));
        MatcherAssert.assertThat(
            "an answer that read a row that grew must be worked out again, but it wasnt",
            asked.get(),
            Matchers.equalTo(2)
        );
    }
}
