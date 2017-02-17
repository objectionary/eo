package org.eolang.compiler;

import org.junit.Test;

import static org.junit.Assert.*;
import static org.hamcrest.Matchers.is;

public class AnEOCCommandArgument {
    @Test
    public void does_not_exist_when_there_is_no_second_input_argument() throws Exception
    {
        assertTrue(
            new EOCCommandArgument(new String[]{"first"}).isEmpty());
    }

    @Test
    public void does_exist_when_there_is_a_second_input_argument() throws Exception
    {
        assertFalse(
            new EOCCommandArgument(new String[]{"first", "second", "third"}).isEmpty());
    }

    @Test
    public void matches_the_value_of_the_second_input_argument() throws Exception
    {
        String secondArgument = "second";

        assertThat(
            new EOCCommandArgument(new String[]{"first", secondArgument, "third"}).string(),
            is(secondArgument));
    }

    @Test
    public void is_an_empty_string_when_there_are_no_input_arguments_at_all() throws Exception
    {
        assertThat(
            new EOCCommandArgument(new String[]{}).string(),
            is(""));
    }

    @Test
    public void is_an_empty_string_when_there_is_no_second_input_argument() throws Exception
    {
        assertThat(
            new EOCCommandArgument(new String[]{"first"}).string(),
            is(""));
    }
}
