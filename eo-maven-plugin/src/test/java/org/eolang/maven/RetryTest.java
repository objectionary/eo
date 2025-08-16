package org.eolang.maven;

import java.io.IOException;
import java.util.concurrent.atomic.AtomicReference;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class RetryTest {

    @Test
    void failsAtReachingLimitsOfRetrys() {
        MatcherAssert.assertThat(
            "Number of failed retrys is not equal with expected 2 fails",
            Assertions.assertThrows(
                IOException.class,
                () ->
                    new Retry<>(
                        () -> {
                            throw new IOException();
                        },
                        2).value(),
                "Exception has hot been thrown"
            ).getMessage(),
            Matchers.equalTo("Failed to execute scalar delegate after 2 trys")
        );
    }

    @Test
    void executesExactlyOnceAtNoError() {
        final AtomicReference<Integer> count = new AtomicReference<>(0);
        MatcherAssert.assertThat(
            "Number of executions is not equal with expected 1 execution",
            Assertions.assertDoesNotThrow(
                () -> new Retry<>(
                    () -> {
                        count.set(1 + count.get());
                        return count.get();
                    },
                    3
                ).value(),
                "Exception has been thrown"
            ),
            Matchers.equalTo(1)
        );
    }


    @Test
    void executesEventually() {
        final AtomicReference<Integer> count = new AtomicReference<>(0);
        MatcherAssert.assertThat(
            "Number of executions is not equal with expected 2 executions",
            Assertions.assertDoesNotThrow(
                () -> new Retry<>(
                    () -> {
                        count.set(1 + count.get());
                        if (2 > count.get()) {
                            throw new IOException();
                        }
                        return count.get();
                    },
                    3
                ).value(),
                "Exception has been thrown"
            ),
            Matchers.equalTo(2)
        );
    }
}
