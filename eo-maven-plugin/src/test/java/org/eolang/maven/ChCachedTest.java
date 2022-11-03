package org.eolang.maven;

import java.util.concurrent.atomic.AtomicInteger;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

class ChCachedTest {

    @Test
    void cachesHashAndInvokesDelegateOnlyOnce() {
        final Dummy delegate = new Dummy();
        final ChCached cached = new ChCached(delegate);
        for (int i = 0; i < 10; i++) {
            final String actual = cached.value();
            MatcherAssert.assertThat(actual, Matchers.equalTo("dummy"));
        }
        MatcherAssert.assertThat(delegate.invocations(), Matchers.equalTo(1));
    }

    private static class Dummy implements CommitHash {

        private final AtomicInteger invocations;

        private Dummy() {
            this.invocations = new AtomicInteger(0);
        }

        @Override
        public String value() {
            invocations.incrementAndGet();
            return "dummy";
        }

        private int invocations() {
            return invocations.get();
        }
    }
}