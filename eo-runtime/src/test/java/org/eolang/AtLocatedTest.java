package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.core.IsEqual;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class AtLocatedTest {

    @Test
    void convertsToString() {
        MatcherAssert.assertThat(
            new AtLocated(new AtSimple(), 10, 10).toString(),
            Matchers.equalTo("<10:10>ΦS")
        );
    }

    @Test
    void getsPhiTermFromOrigin() {
        final AtSimple origin = new AtSimple();
        MatcherAssert.assertThat(
            new AtLocated(origin, 10, 10).φTerm(),
            Matchers.equalTo(origin.φTerm())
        );

    }

    @Test
    void copies() {
        final AtLocated origin = new AtLocated(new AtSimple(), 10, 10);
        MatcherAssert.assertThat(
            origin.copy(Phi.Φ).toString(),
            Matchers.equalTo(origin.toString())
        );

    }

    @Test
    void rethrowsExFlowException() {
        Assertions.assertThrows(
            ExFlow.class,
            () -> new AtLocated(new AtFailed(), 10, 10).get()
        );
    }

    @Test
    void rethrowsExFailure() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new AtLocated(new AtFailed(new ExUnset("Unset")), 10, 10).get()
        );
    }

    @Test
    void putsWithExFlowException() {
        Assertions.assertThrows(
            ExFlow.class,
            () -> new AtLocated(new AtFailed(), 10, 10).put(Phi.Φ)
        );
    }

    private static class AtFailed implements Attr {

        private final ExAbstract exception;

        AtFailed() {
            this(new ExFlow());
        }

        public AtFailed(final ExAbstract ex) {
            this.exception = ex;
        }

        @Override
        public Attr copy(final Phi self) {
            throw exception;
        }

        @Override
        public Phi get() {
            throw exception;
        }

        @Override
        public void put(final Phi phi) {
            throw exception;

        }

        @Override
        public String φTerm() {
            throw exception;
        }
    }
}