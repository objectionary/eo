package org.eolang.test;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

public final class CheckXSLTest {

    @Test
    void check() {
        MatcherAssert.assertThat(true, Matchers.is(true));
    }

}
