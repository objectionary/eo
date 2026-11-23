/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven.dealpha;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.SecureRandom;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test cases for {@link Grounded}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class GroundedTest {

    @Test
    void groundsNoArgumentOfCallOnVoid(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "an argument of a call on a void must not be grounded, but it is",
            new Grounded(
                GroundedTest.facts(
                    temp,
                    "<objects><type id='Φ.guard'>",
                    "<attr name='fail' type='Φ.guard.fail' void='true'/>",
                    "<attr name='φ' type='Φ.guard.φ'/>",
                    "</type></objects>",
                    "<reads><read id='Φ.guard.φ' name='fail' of='Φ.guard.φ.ρ'/></reads>",
                    "<heads><type id='Φ.guard.φ.ρ'><ref loc='Φ.guard'/></type></heads>",
                    "<calls><call loc='Φ.guard.φ'><arg loc='Φ.guard.φ.α0' n='0'/></call></calls>"
                )
            ).arguments(),
            Matchers.not(Matchers.hasItem("Φ.guard.φ.α0"))
        );
    }

    @Test
    void groundsArgumentOfCallOnVoidFilledWithSameVoids(@Mktmp final Path temp)
        throws IOException {
        MatcherAssert.assertThat(
            "an argument of a call on a void every caller fills alike must be grounded, but it isnt",
            new Grounded(
                GroundedTest.facts(
                    temp,
                    "<objects><type id='Φ.guard'>",
                    "<attr name='fail' type='Φ.guard.fail' void='true'/>",
                    "<attr name='φ' type='Φ.guard.φ'/>",
                    "</type><type id='Φ.app'><attr name='φ' type='Φ.app.φ'/></type>",
                    "<type id='Φ.app.φ.α0'>",
                    "<attr name='message' type='Φ.app.φ.α0.message' void='true'/>",
                    "</type></objects>",
                    "<reads><read id='Φ.guard.φ' name='fail' of='Φ.guard.φ.ρ'/></reads>",
                    "<heads><type id='Φ.guard.φ.ρ'><ref loc='Φ.guard'/></type>",
                    "<type id='Φ.app.φ'><ref loc='Φ.guard'/></type></heads>",
                    "<calls><call loc='Φ.guard.φ'><arg loc='Φ.guard.φ.α0' n='0'/></call>",
                    "<call loc='Φ.app.φ'><arg loc='Φ.app.φ.α0' n='0'/></call></calls>"
                )
            ).arguments(),
            Matchers.hasItem("Φ.guard.φ.α0")
        );
    }

    @Test
    void groundsArgumentOfCallBehindVoidTheEngineAnswers(@Mktmp final Path temp)
        throws IOException {
        Files.write(
            temp.resolve("answers.xml"),
            String.join(
                System.lineSeparator(),
                "<answers>",
                "  <o loc='Φ.number.φ' copies='1'>",
                "    <is loc='Φ.bytes'/>",
                "  </o>",
                "</answers>"
            ).getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "an argument of a call behind a void the engine saw hold one formation must be grounded, but it isnt",
            new Grounded(GroundedTest.numbers(temp)).arguments(),
            Matchers.hasItem("Φ.app.φ.α0")
        );
    }

    @Test
    void groundsNoArgumentOfCallBehindVoidTheEngineAnswersTwice(@Mktmp final Path temp)
        throws IOException {
        Files.write(
            temp.resolve("answers.xml"),
            String.join(
                "",
                "<answers><o loc='Φ.number.φ' copies='2'><is loc='Φ.bytes'/>",
                "<terminator/></o></answers>"
            ).getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "an argument of a call behind a void the engine also saw terminated must not be grounded, but it is",
            new Grounded(GroundedTest.numbers(temp)).arguments(),
            Matchers.not(Matchers.hasItem("Φ.app.φ.α0"))
        );
    }

    @Test
    void groundsArgumentOfCallThroughRho(@Mktmp final Path temp) throws IOException {
        final String name = String.format("make%d", new SecureRandom().nextInt(10_000));
        MatcherAssert.assertThat(
            "an argument of a call on a formation reached through ρ must be grounded, but it isnt",
            new Grounded(
                GroundedTest.facts(
                    temp,
                    "<objects><type id='Φ.box'>",
                    String.format("<attr name='%1$s' type='Φ.box.%1$s'/>", name),
                    "<attr name='leaf' type='Φ.box.leaf'/>",
                    "</type>",
                    String.format("<type id='Φ.box.%s'>", name),
                    String.format("<attr name='k' type='Φ.box.%s.k' void='true'/>", name),
                    "</type><type id='Φ.box.leaf'>",
                    "<attr name='ρ' rho='Φ.box' type='Φ.box.leaf.ρ' void='true'/>",
                    "<attr name='φ' type='Φ.box.leaf.φ'/>",
                    "</type></objects><reads>",
                    String.format("<read id='Φ.box.leaf.φ' name='%s' of='Φ.box.leaf.φ.ρ'/>", name),
                    "<read id='Φ.box.leaf.φ.ρ' name='ρ' of='Φ.box.leaf.φ.ρ.ρ'/>",
                    "</reads>",
                    "<heads><type id='Φ.box.leaf.φ.ρ.ρ'><ref loc='Φ.box.leaf'/></type></heads>",
                    "<calls><call loc='Φ.box.leaf.φ'><arg loc='Φ.box.leaf.φ.α0' n='0'/></call></calls>"
                )
            ).arguments(),
            Matchers.hasItem("Φ.box.leaf.φ.α0")
        );
    }

    @Test
    void groundsArgumentOfCallOnWhatAtomReturns(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "an argument of a call on what an atom says it returns must be grounded, but it isnt",
            new Grounded(
                GroundedTest.facts(
                    temp,
                    "<objects><type id='Φ'>",
                    "<attr name='dataized' type='Φ.dataized'/>",
                    "<attr name='bytes' type='Φ.bytes'/>",
                    "</type><type id='Φ.dataized' returns='Φ.bytes'>",
                    "<attr name='target' type='Φ.dataized.target' void='true'/>",
                    "</type><type id='Φ.bytes'><attr name='eq' type='Φ.bytes.eq'/></type>",
                    "<type id='Φ.bytes.eq'><attr name='x' type='Φ.bytes.eq.x' void='true'/></type>",
                    "</objects><reads>",
                    "<read id='Φ.app.φ' name='eq' of='Φ.app.φ.ρ'/>",
                    "<read id='Φ.app.φ.ρ' name='dataized' of='Φ.app.φ.ρ.ρ'/>",
                    "</reads>",
                    "<heads><type id='Φ.app.φ.ρ.ρ'><ref loc='Φ'/></type></heads>",
                    "<calls><call loc='Φ.app.φ'><arg loc='Φ.app.φ.α0' n='0'/></call></calls>"
                )
            ).arguments(),
            Matchers.hasItem("Φ.app.φ.α0")
        );
    }

    @Test
    void groundsArgumentOfCallOnTheHeadOfTreeAtomReturns(@Mktmp final Path temp)
        throws IOException {
        final String name = String.format("rt%d", new SecureRandom().nextInt(10_000));
        MatcherAssert.assertThat(
            "an argument of a call on the head of a tree an atom returns must be grounded, but it isnt",
            new Grounded(
                GroundedTest.facts(
                    temp,
                    "<objects><type id='Φ'>",
                    "<attr name='check' type='Φ.check'/>",
                    String.format("<attr name='%1$s' type='Φ.%1$s'/>", name),
                    String.format(
                        "</type><type id='Φ.check' returns='Φ.%s(Φ.number,Φ.string)'>", name
                    ),
                    "</type>",
                    String.format("<type id='Φ.%s'>", name),
                    String.format("<attr name='code' type='Φ.%s.code' void='true'/>", name),
                    String.format("<attr name='retry' type='Φ.%s.retry'/>", name),
                    String.format("</type><type id='Φ.%s.retry'>", name),
                    String.format("<attr name='times' type='Φ.%s.retry.times' void='true'/>", name),
                    "</type></objects><reads>",
                    "<read id='Φ.app.φ' name='retry' of='Φ.app.φ.ρ'/>",
                    "<read id='Φ.app.φ.ρ' name='check' of='Φ.app.φ.ρ.ρ'/>",
                    "</reads>",
                    "<heads><type id='Φ.app.φ.ρ.ρ'><ref loc='Φ'/></type></heads>",
                    "<calls><call loc='Φ.app.φ'><arg loc='Φ.app.φ.α0' n='0'/></call></calls>"
                )
            ).arguments(),
            Matchers.hasItem("Φ.app.φ.α0")
        );
    }

    @Test
    void groundsArgumentOfCallOnVoidAtomSaysJavaFills(@Mktmp final Path temp)
        throws IOException {
        final String name = String.format("st%d", new SecureRandom().nextInt(10_000));
        MatcherAssert.assertThat(
            "an argument of a call on a void an atom types with its brackets must be grounded, but it isnt",
            new Grounded(
                GroundedTest.facts(
                    temp,
                    "<objects><type id='Φ'>",
                    "<attr name='check' type='Φ.check'/>",
                    String.format("<attr name='%1$s' type='Φ.%1$s'/>", name),
                    "<attr name='number' type='Φ.number'/>",
                    String.format(
                        "</type><type id='Φ.check' returns='Φ.%s(Φ.string,Φ.number)'>", name
                    ),
                    String.format("</type><type id='Φ.%s'>", name),
                    String.format("<attr name='ρ' rho='Φ' type='Φ.%s.ρ' void='true'/>", name),
                    String.format("<attr name='msg' type='Φ.%s.msg' void='true'/>", name),
                    String.format("<attr name='cde' type='Φ.%s.cde' void='true'/>", name),
                    String.format("<attr name='code' type='Φ.%s.code'/>", name),
                    "</type><type id='Φ.number'><attr name='plus' type='Φ.number.plus'/></type>",
                    "<type id='Φ.number.plus'><attr name='y' type='Φ.number.plus.y' void='true'/>",
                    "</type></objects><reads>",
                    String.format("<read id='Φ.%1$s.code' name='cde' of='Φ.%1$s.code.ρ'/>", name),
                    "<read id='Φ.app.φ' name='plus' of='Φ.app.φ.ρ'/>",
                    "<read id='Φ.app.φ.ρ' name='code' of='Φ.app.φ.ρ.ρ'/>",
                    "<read id='Φ.app.φ.ρ.ρ' name='check' of='Φ.app.φ.ρ.ρ.ρ'/>",
                    "</reads><heads>",
                    String.format("<type id='Φ.%1$s.code.ρ'><ref loc='Φ.%1$s'/></type>", name),
                    "<type id='Φ.app.φ.ρ.ρ.ρ'><ref loc='Φ'/></type>",
                    "</heads>",
                    "<calls><call loc='Φ.app.φ'><arg loc='Φ.app.φ.α0' n='0'/></call></calls>"
                )
            ).arguments(),
            Matchers.hasItem("Φ.app.φ.α0")
        );
    }

    @Test
    void groundsArgumentOfCallOnVoidThatCallInEoFillsByName(@Mktmp final Path temp)
        throws IOException {
        final String name = String.format("st%d", new SecureRandom().nextInt(10_000));
        MatcherAssert.assertThat(
            "an argument of a call on a void a call in EO fills by name must be grounded, but it isnt",
            new Grounded(
                GroundedTest.facts(
                    temp,
                    "<objects><type id='Φ'>",
                    String.format("<attr name='%1$s' type='Φ.%1$s'/>", name),
                    "<attr name='number' type='Φ.number'/>",
                    "<attr name='string' type='Φ.string'/>",
                    String.format("</type><type id='Φ.%s'>", name),
                    String.format("<attr name='ρ' rho='Φ' type='Φ.%s.ρ' void='true'/>", name),
                    String.format("<attr name='msg' type='Φ.%s.msg' void='true'/>", name),
                    String.format("<attr name='cde' type='Φ.%s.cde' void='true'/>", name),
                    String.format("<attr name='code' type='Φ.%s.code'/>", name),
                    "</type><type id='Φ.number'><attr name='plus' type='Φ.number.plus'/></type>",
                    "<type id='Φ.number.plus'><attr name='y' type='Φ.number.plus.y' void='true'/>",
                    "</type><type id='Φ.string'/></objects><reads>",
                    String.format("<read id='Φ.%1$s.code' name='cde' of='Φ.%1$s.code.ρ'/>", name),
                    "<read id='Φ.app.φ' name='plus' of='Φ.app.φ.ρ'/>",
                    "<read id='Φ.app.φ.ρ' name='code' of='Φ.app.φ.ρ.ρ'/>",
                    String.format("<read id='Φ.app.φ.ρ.ρ' name='%s' of='Φ.app.φ.ρ.ρ.ρ'/>", name),
                    "<read id='Φ.app.φ.ρ.ρ.cde' name='number' of='Φ.app.φ.ρ.ρ.cde.ρ'/>",
                    "<read id='Φ.app.φ.ρ.ρ.msg' name='string' of='Φ.app.φ.ρ.ρ.msg.ρ'/>",
                    "</reads><heads>",
                    String.format("<type id='Φ.%1$s.code.ρ'><ref loc='Φ.%1$s'/></type>", name),
                    "<type id='Φ.app.φ.ρ.ρ.ρ'><ref loc='Φ'/></type>",
                    "<type id='Φ.app.φ.ρ.ρ.cde.ρ'><ref loc='Φ'/></type>",
                    "<type id='Φ.app.φ.ρ.ρ.msg.ρ'><ref loc='Φ'/></type>",
                    "</heads><calls><call loc='Φ.app.φ.ρ.ρ'>",
                    "<arg loc='Φ.app.φ.ρ.ρ.cde' name='cde'/>",
                    "<arg loc='Φ.app.φ.ρ.ρ.msg' name='msg'/>",
                    "</call><call loc='Φ.app.φ'><arg loc='Φ.app.φ.α0' n='0'/></call></calls>"
                )
            ).arguments(),
            Matchers.hasItem("Φ.app.φ.α0")
        );
    }

    @Test
    void groundsArgumentOfCallOnWhatGenericAtomReturns(@Mktmp final Path temp)
        throws IOException {
        MatcherAssert.assertThat(
            "an argument of a call on what a generic atom returns must be grounded when all its returned voids agree, but it isnt",
            new Grounded(
                GroundedTest.facts(
                    temp,
                    "<objects><type id='Φ'>",
                    "<attr name='g' type='Φ.g'/><attr name='number' type='Φ.number'/>",
                    "</type><type id='Φ.g' returns='Φ.g.v'>",
                    "<attr name='v' type='Φ.g.v' void='true'/>",
                    "<attr name='a' type='Φ.g.a' void='true'/>",
                    "<return loc='Φ.g.v'/><return loc='Φ.g.a'/>",
                    "</type><type id='Φ.number'><attr name='eq' type='Φ.number.eq'/></type>",
                    "<type id='Φ.number.eq'><attr name='b' type='Φ.number.eq.b' void='true'/>",
                    "</type></objects><reads>",
                    "<read id='Φ.app.φ' name='eq' of='Φ.app.φ.ρ'/>",
                    "<read id='Φ.app.φ.ρ' name='g' of='Φ.app.φ.ρ.ρ'/>",
                    "<read id='Φ.app.φ.ρ.α0' name='number' of='Φ.app.φ.ρ.α0.ρ'/>",
                    "<read id='Φ.app.φ.ρ.α1' name='number' of='Φ.app.φ.ρ.α1.ρ'/>",
                    "</reads><heads>",
                    "<type id='Φ.app.φ.ρ.ρ'><ref loc='Φ'/></type>",
                    "<type id='Φ.app.φ.ρ.α0.ρ'><ref loc='Φ'/></type>",
                    "<type id='Φ.app.φ.ρ.α1.ρ'><ref loc='Φ'/></type>",
                    "</heads><calls><call loc='Φ.app.φ.ρ'>",
                    "<arg loc='Φ.app.φ.ρ.α0' n='0'/><arg loc='Φ.app.φ.ρ.α1' n='1'/>",
                    "</call><call loc='Φ.app.φ'><arg loc='Φ.app.φ.α0' n='0'/></call></calls>"
                )
            ).arguments(),
            Matchers.hasItem("Φ.app.φ.α0")
        );
    }

    @Test
    void groundsArgumentOfCallOnForkWithTerminatorSide(@Mktmp final Path temp)
        throws IOException {
        MatcherAssert.assertThat(
            "an argument of a call read off a fork whose other side is a terminator must be grounded, but it isnt",
            new Grounded(GroundedTest.forked(temp, "<terminator/>")).arguments(),
            Matchers.hasItem("Φ.app.φ.α0")
        );
    }

    @Test
    void groundsNoArgumentOfCallOnForkWithUnknownSide(@Mktmp final Path temp)
        throws IOException {
        MatcherAssert.assertThat(
            "an argument of a call read off a fork whose other side is unknown must not be grounded, but it is",
            new Grounded(GroundedTest.forked(temp, "<data/>")).arguments(),
            Matchers.not(Matchers.hasItem("Φ.app.φ.α0"))
        );
    }

    @Test
    void groundsNoArgumentOfTerminator(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "an argument of a terminator must not be grounded, but it is",
            new Grounded(GroundedTest.forked(temp, "<terminator/>")).arguments(),
            Matchers.not(Matchers.hasItem("Φ.app.φ.ρ.α0.α0"))
        );
    }

    @Test
    void groundsArgumentOfReadOffHelperWhoseOtherSideIsHelperItself(@Mktmp final Path temp)
        throws IOException {
        MatcherAssert.assertThat(
            "a read off a recursive helper must be found on the side that does not lead back to it, but it isnt",
            new Grounded(
                GroundedTest.looping(
                    temp, "", "<type id='Φ.grow.φ.α1'><ref loc='Φ.grow'/></type>"
                )
            ).arguments(),
            Matchers.hasItem("Φ.app.φ.α0")
        );
    }

    @Test
    void groundsNoArgumentOfReadOffHelperWhoseOtherSideReadsOffHelper(@Mktmp final Path temp)
        throws IOException {
        MatcherAssert.assertThat(
            "a read off a recursive helper must not be found when its other side reads off the helper again, but it is",
            new Grounded(
                GroundedTest.looping(
                    temp,
                    "<read id='Φ.grow.φ.α1' name='g' of='Φ.grow.φ.α1.ρ'/>",
                    "<type id='Φ.grow.φ.α1.ρ'><ref loc='Φ.grow'/></type>"
                )
            ).arguments(),
            Matchers.not(Matchers.hasItem("Φ.app.φ.α0"))
        );
    }

    private static Path forked(final Path dir, final String side) throws IOException {
        return GroundedTest.facts(
            dir,
            "<objects><type id='Φ.bool'>",
            "<attr name='fork' type='Φ.bool.fork' void='true'/>",
            "<attr name='if' type='Φ.bool.if'/></type>",
            "<type id='Φ.true'><attr name='φ' type='Φ.true.φ'/></type>",
            "<type id='Φ.false'><attr name='φ' type='Φ.false.φ'/></type>",
            "<type id='Φ.true.φ.α0'><attr name='left' type='Φ.true.φ.α0.left' void='true'/>",
            "<attr name='right' type='Φ.true.φ.α0.right' void='true'/>",
            "<attr name='φ' type='Φ.true.φ.α0.φ'/></type>",
            "<type id='Φ.false.φ.α0'><attr name='left' type='Φ.false.φ.α0.left' void='true'/>",
            "<attr name='right' type='Φ.false.φ.α0.right' void='true'/>",
            "<attr name='φ' type='Φ.false.φ.α0.φ'/></type>",
            "<type id='Φ.num'><attr name='minus' type='Φ.num.minus'/></type>",
            "<type id='Φ.num.minus'><attr name='x' type='Φ.num.minus.x' void='true'/></type>",
            "<type id='Φ.app'><attr name='φ' type='Φ.app.φ'/></type></objects>",
            "<reads><read id='Φ.true.φ.α0.φ' name='left' of='Φ.true.φ.α0.φ.ρ'/>",
            "<read id='Φ.false.φ.α0.φ' name='right' of='Φ.false.φ.α0.φ.ρ'/>",
            "<read id='Φ.bool.if' name='fork' of='Φ.bool.if.ρ'/>",
            "<read id='Φ.app.φ' name='minus' of='Φ.app.φ.ρ'/>",
            "<read id='Φ.app.φ.ρ' name='if' of='Φ.app.φ.ρ.ρ'/></reads>",
            "<heads><type id='Φ.true.φ'><ref loc='Φ.bool'/></type>",
            "<type id='Φ.false.φ'><ref loc='Φ.bool'/></type>",
            "<type id='Φ.true.φ.α0.φ.ρ'><ref loc='Φ.true.φ.α0'/></type>",
            "<type id='Φ.false.φ.α0.φ.ρ'><ref loc='Φ.false.φ.α0'/></type>",
            "<type id='Φ.bool.if.ρ'><ref loc='Φ.bool'/></type>",
            "<type id='Φ.app.φ.ρ.ρ'><ref loc='Φ.bool'/></type>",
            String.format("<type id='Φ.app.φ.ρ.α0'>%s</type>", side),
            "<type id='Φ.app.φ.ρ.α1'><ref loc='Φ.num'/></type></heads>",
            "<calls><call loc='Φ.true.φ'><arg loc='Φ.true.φ.α0' n='0'/></call>",
            "<call loc='Φ.false.φ'><arg loc='Φ.false.φ.α0' n='0'/></call>",
            "<call loc='Φ.app.φ.ρ.α0'><arg loc='Φ.app.φ.ρ.α0.α0' n='0'/></call>",
            "<call loc='Φ.app.φ.ρ'><arg loc='Φ.app.φ.ρ.α0' n='0'/>",
            "<arg loc='Φ.app.φ.ρ.α1' n='1'/></call>",
            "<call loc='Φ.app.φ'><arg loc='Φ.app.φ.α0' n='0'/></call></calls>"
        );
    }

    private static Path looping(final Path dir, final String read, final String side)
        throws IOException {
        return GroundedTest.facts(
            dir,
            "<objects><type id='Φ.bool'>",
            "<attr name='fork' type='Φ.bool.fork' void='true'/>",
            "<attr name='if' type='Φ.bool.if'/></type>",
            "<type id='Φ.true'><attr name='φ' type='Φ.true.φ'/></type>",
            "<type id='Φ.false'><attr name='φ' type='Φ.false.φ'/></type>",
            "<type id='Φ.true.φ.α0'><attr name='left' type='Φ.true.φ.α0.left' void='true'/>",
            "<attr name='right' type='Φ.true.φ.α0.right' void='true'/>",
            "<attr name='φ' type='Φ.true.φ.α0.φ'/></type>",
            "<type id='Φ.false.φ.α0'><attr name='left' type='Φ.false.φ.α0.left' void='true'/>",
            "<attr name='right' type='Φ.false.φ.α0.right' void='true'/>",
            "<attr name='φ' type='Φ.false.φ.α0.φ'/></type>",
            "<type id='Φ.grow'><attr name='φ' type='Φ.grow.φ'/></type>",
            "<type id='Φ.yes'><attr name='g' type='Φ.yes.g'/></type>",
            "<type id='Φ.yes.g'><attr name='x' type='Φ.yes.g.x' void='true'/>",
            "<attr name='g' type='Φ.yes.g.g'/></type>",
            "<type id='Φ.yes.g.g'><attr name='y' type='Φ.yes.g.g.y' void='true'/>",
            "<attr name='z' type='Φ.yes.g.g.z' void='true'/></type>",
            "<type id='Φ.app'><attr name='φ' type='Φ.app.φ'/></type></objects>",
            "<reads><read id='Φ.true.φ.α0.φ' name='left' of='Φ.true.φ.α0.φ.ρ'/>",
            "<read id='Φ.false.φ.α0.φ' name='right' of='Φ.false.φ.α0.φ.ρ'/>",
            "<read id='Φ.bool.if' name='fork' of='Φ.bool.if.ρ'/>",
            "<read id='Φ.grow.φ' name='if' of='Φ.grow.φ.ρ'/>",
            read,
            "<read id='Φ.app.φ' name='g' of='Φ.app.φ.ρ'/></reads>",
            "<heads><type id='Φ.true.φ'><ref loc='Φ.bool'/></type>",
            "<type id='Φ.false.φ'><ref loc='Φ.bool'/></type>",
            "<type id='Φ.true.φ.α0.φ.ρ'><ref loc='Φ.true.φ.α0'/></type>",
            "<type id='Φ.false.φ.α0.φ.ρ'><ref loc='Φ.false.φ.α0'/></type>",
            "<type id='Φ.bool.if.ρ'><ref loc='Φ.bool'/></type>",
            "<type id='Φ.grow.φ.ρ'><ref loc='Φ.bool'/></type>",
            "<type id='Φ.grow.φ.α0'><ref loc='Φ.yes'/></type>",
            side,
            "<type id='Φ.app.φ.ρ'><ref loc='Φ.grow'/></type></heads>",
            "<calls><call loc='Φ.true.φ'><arg loc='Φ.true.φ.α0' n='0'/></call>",
            "<call loc='Φ.false.φ'><arg loc='Φ.false.φ.α0' n='0'/></call>",
            "<call loc='Φ.grow.φ'><arg loc='Φ.grow.φ.α0' n='0'/>",
            "<arg loc='Φ.grow.φ.α1' n='1'/></call>",
            "<call loc='Φ.app.φ'><arg loc='Φ.app.φ.α0' n='0'/></call></calls>"
        );
    }

    private static Path numbers(final Path dir) throws IOException {
        return GroundedTest.facts(
            dir,
            "<objects><type id='Φ'>",
            "<attr name='number' type='Φ.number'/><attr name='bytes' type='Φ.bytes'/>",
            "<attr name='app' type='Φ.app'/></type>",
            "<type id='Φ.number'><attr name='φ' type='Φ.number.φ' void='true'/></type>",
            "<type id='Φ.bytes'><attr name='eq' type='Φ.bytes.eq'/></type>",
            "<type id='Φ.bytes.eq'><attr name='b' type='Φ.bytes.eq.b' void='true'/></type>",
            "<type id='Φ.app'><attr name='φ' type='Φ.app.φ'/></type></objects>",
            "<reads><read id='Φ.app.φ' name='eq' of='Φ.app.φ.ρ'/></reads>",
            "<heads><type id='Φ.app.φ.ρ'><ref loc='Φ.number'/></type></heads>",
            "<calls><call loc='Φ.app.φ'><arg loc='Φ.app.φ.α0' n='0'/></call></calls>"
        );
    }

    private static Path facts(final Path dir, final String... parts) throws IOException {
        final Path table = dir.resolve("facts.xml");
        Files.write(
            table,
            String.format("<facts>%s</facts>", String.join("", parts))
                .getBytes(StandardCharsets.UTF_8)
        );
        return table;
    }
}
