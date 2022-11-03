package org.eolang.maven;

import java.util.concurrent.atomic.AtomicInteger;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

class ChCachedTest {

    @Test
    void cachesHashAndInvokesDelegateOnlyOnce() {
        final ChMock mock = new ChMock();
        final ChCached cached = new ChCached(mock);
        for (int idx = 0; idx < 10; idx++) {
            MatcherAssert.assertThat(cached.value(), Matchers.equalTo("dummy"));
        }
        MatcherAssert.assertThat(mock.invocations(), Matchers.equalTo(1));
    }

    private static class ChMock implements CommitHash {

        private final AtomicInteger invocations;

        private ChMock() {
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