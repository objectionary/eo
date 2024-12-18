package org.eolang.parser;

import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case for {@link UnderlinedMessage}.
 * @since 0.1
 */
final class UnderlinedMessageTest {

    @ParameterizedTest
    @MethodSource("examples")
    void addsUndeline(final String input, final int from, final int length, final String expected) {
        MatcherAssert.assertThat(
            "We expect the message to be highlighted with underline characters",
            new UnderlinedMessage(input, from, length).formatted(),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void throwsExceptionWhenUnderlineIsOutOfBounds() {
        MatcherAssert.assertThat(
            "We expect the exception to have a meaningful message",
            Assertions.assertThrows(
                IllegalArgumentException.class,
                () -> new UnderlinedMessage("Hello, world!", 0, 100).formatted(),
                "We expect an exception to be thrown when underline is out of bounds"
            ).getMessage(),
            Matchers.equalTo(
                "The underline is out of bounds: from=0, length=100 for 'Hello, world!'(13)"
            )
        );
    }

    @Test
    void throwsExceptionWhenLengthIsLessThanZero() {
        MatcherAssert.assertThat(
            "We expect the exception to have a meaningful message in case of negative length",
            Assertions.assertThrows(
                IllegalArgumentException.class,
                () -> new UnderlinedMessage("Hello, world!", 0, -1).formatted(),
                "We expect an exception to be thrown when length is less than zero"
            ).getMessage(),
            Matchers.equalTo(
                "The underline is out of bounds: from=0, length=-1 for 'Hello, world!'(13)"
            )
        );
    }

    /**
     * Test cases for {@link UnderlinedMessageTest#addsUndeline}.
     * @return Test cases.
     */
    static Stream<Arguments> examples() {
        final String issue = "Problem is here";
        return Stream.of(
            Arguments.of(issue, 0, 7, "Problem is here\n^^^^^^^        "),
            Arguments.of(issue, 8, 2, "Problem is here\n        ^^     "),
            Arguments.of(issue, 0, 0, "Problem is here\n               "),
            Arguments.of(issue, 0, 1, "Problem is here\n^              "),
            Arguments.of(issue, 14, 1, "Problem is here\n              ^")
        );
    }

}