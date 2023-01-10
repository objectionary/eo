package org.eolang;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class AtLoggedTest {

    private AtLogged logged;
    private AtSimple origin;
    private ByteArrayOutputStream out;
    private String label;

    @BeforeEach
    void setUp() {
        out = new ByteArrayOutputStream();
        origin = new AtSimple();
        label = "test";
        logged = new AtLogged(origin, label, new PrintStream(out));
    }

    @Test
    void convertsToStringAsOrigin() {
        MatcherAssert.assertThat(
            logged.toString(),
            Matchers.equalTo(origin.toString())
        );
    }

    @Test
    void convertsToPhiTermAsOrigin() {
        MatcherAssert.assertThat(
            logged.φTerm(),
            Matchers.equalTo(origin.φTerm())
        );
    }

    @Test
    void copiesWithLogging() {
        logged.copy(Phi.Φ);
        final String log = new String(out.toByteArray(), StandardCharsets.UTF_8);
        MatcherAssert.assertThat(
            log,
            Matchers.containsString(String.format("  %s.copy()...", label))
        );
        MatcherAssert.assertThat(
            log,
            Matchers.containsString(String.format("  %s.copy()!", label))
        );
    }

    @Test
    void getsWithLogging() {
        MatcherAssert.assertThat(
            logged.get(),
            Matchers.equalTo(origin.get())
        );
        final String log = new String(out.toByteArray(), StandardCharsets.UTF_8);
        MatcherAssert.assertThat(
            log,
            Matchers.containsString(String.format("  %s.get()...", label))
        );
        MatcherAssert.assertThat(
            log,
            Matchers.containsString(String.format("  %s.get()!", label))
        );
    }

    @Test
    void putsWithLogging() {
        logged.put(Phi.Φ);
        final String log = new String(out.toByteArray(), StandardCharsets.UTF_8);
        MatcherAssert.assertThat(
            log,
            Matchers.containsString(String.format("  %s.put()...", label))
        );
        MatcherAssert.assertThat(
            log,
            Matchers.containsString(String.format("  %s.put()!", label))
        );
    }
}