package org.eolang.maven.objectionary;

import org.eolang.maven.OnlineCondition;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

class ObjectsIndexTest {

    @Test
    void contains() {
    }


    @Test
    @ExtendWith(OnlineCondition.class)
    void downloadsAndChecks() {
        MatcherAssert.assertThat(
            new ObjectsIndex().contains("org.eolang.io.stdout"),
            Matchers.is(true)
        );
    }
}