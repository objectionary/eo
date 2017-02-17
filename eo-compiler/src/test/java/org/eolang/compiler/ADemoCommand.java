package org.eolang.compiler;

import org.junit.Test;

import static java.util.Arrays.asList;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

public class ADemoCommand {

    @Test
    public void with_no_command_argument_outputs_instructions() throws Exception
    {
        EOCCommandArgument commandArgument =
            new EOCCommandArgument(new String[]{});

        assertThat(
            commandArgument.isEmpty(),
            is(true)
        );

        assertThat(
            new DemoCommand(commandArgument, "org//eolang//compiler//eo").output(),
            containsString("Use --demo <filename> to see one of the following files compiled:")
        );
    }

    @Test
    public void with_no_command_argument_outputs_the_list_of_eo_files() throws Exception
    {
        EOCCommandArgument commandArgument =
            new EOCCommandArgument(new String[]{});

        assertThat(
            commandArgument.isEmpty(),
            is(true)
        );

        assertThat(
            new DemoCommand(commandArgument, "org//eolang//compiler//eo").output(),
            stringContainsInOrder(asList("book.eo", "car.eo", "zero.eo"))
        );
    }
    @Test

    public void with_a_command_argument_for_a_nonexistent_file_does_what() throws Exception
    {
        EOCCommandArgument nonexistentFile =
            new EOCCommandArgument(new String[]{"name", "blah.blah"});

        assertThat(
            new DemoCommand(nonexistentFile, "org//eolang//compiler//eo").output(),
            containsString("Error reading resource file.")
        );
    }
}