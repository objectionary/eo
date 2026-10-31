/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.aspects.RetryOnFailure;
import com.jcabi.log.Logger;
import java.io.IOException;
import java.net.URL;
import java.util.concurrent.TimeUnit;
import org.cactoos.Text;
import org.cactoos.text.Sticky;
import org.cactoos.text.Synced;
import org.cactoos.text.TextEnvelope;
import org.cactoos.text.TextOf;

/**
 * Commit hashes table as text from objectionary.
 *
 * <p>This class serves the purpose of the global cache in order to avoid
 * downloading the list of tags multiple times from objectionary.</p>
 *
 * @since 0.29.6
 */
final class CommitHashesText extends TextEnvelope {

    /**
     * Fallback hashes, a copy of
     * <a href="https://home.objectionary.com/tags.txt">tags.txt</a>.
     *
     * <p>The table must hold every tag the live file holds, {@code master}
     * among them, because {@code master} is the default {@code eo.tag}. A
     * fallback without it turns an unreachable {@code home.objectionary.com}
     * into a complaint about a tag that does exist.</p>
     */
    private static final String FALLBACK = String.join(
        System.lineSeparator(),
        "ee14a1e30a9e0f8f64404e7a52f40c2a07f88359 0.40.5",
        "a1037ce5238c5556006c34961961ff28c1ad7e84 0.41.0",
        "9a3dee39597b2e9ac305ca57c296b0fa7e10eb55 0.41.1",
        "85ef55a0b793dd933786a4b82f3467f59e9338ec 0.41.2",
        "710f25458d8ff327d8c76720044b1ef16aeddf05 0.41.3",
        "25eb1fbad0022b91c785e7dca1e0d3fdf102a2d5 0.42.0",
        "0ee62a805ca88c29a4a6c9cb9cb65ebb5cea12a2 0.43.0",
        "0d7aeeedd31d8f1fe01efa38f0bb1764f096bf1f 0.43.1",
        "ac333e9880afddeec62d5014fb40a6005ee59abb 0.43.2",
        "cc47a714882c7e1cea2d546190f27bd39fe1d067 0.44.0",
        "f7fc58402f2efe0bda75d9d5e69b6e9ba718d91a 0.45.0",
        "8561eb3cbc8e977be7f87c17a2b47f7ca8911d9e 0.46.0",
        "1c2b3edaf693988c44b6fab856738f233e78da18 0.48.1",
        "ffd9f5985f109147f7478119d49bb3b6061b5da6 0.48.2",
        "a1721a1f4f917186da4d8cdf6ec3979c6285cbd5 0.49.0",
        "9151c4d22f99a7e2128dff0c6b60d9d3d8307676 0.49.1",
        "254c851959afb8f5479e030dc7ce061acbbc4b07 0.49.2",
        "85650be629871fcbf3a939b17a2473e8f614e879 0.49.3",
        "4c03d94ac7a0cbb0bde266d8edacbe5eecd2bc6f 0.49.4",
        "658b6d9eef1307c63c3e63e3fff3d4a71802ba6c 0.50.0",
        "7306ad8abc779762baf59e6526e26ac0f795cf38 0.50.1",
        "6100f860647dac803ba4270ae8d1d1f6d4090de8 0.50.2",
        "425e44e9777cf9340be6f0b9afd46a00855c3335 0.51.3",
        "f73528a558bb597e604dcbfc1dce0933eab334ee 0.51.6",
        "3744a0780091d265096479145544ec7fce7b17a6 0.52.0",
        "fddde24f402acd9a17b439fae24b05742c08b064 0.53.0",
        "e62d7733c102bc3de940ca02f3e04bae26ba1818 0.55.0",
        "ce6e0a71e41c08cae7c073462343f1a306621077 0.55.1",
        "aa355f26077e2761ffd5c08928c93e0197fb89e9 0.55.2",
        "92e4098fe78fe21286c8446649c34261e7d94cde 0.56.0",
        "84d7ff4ecc632bfea581c96dee02636f81bc418d 0.56.2",
        "ca64924a3aea4d0c594ec059e1da380cf4e16ce5 0.56.4",
        "2010eb8edbcd99c8d5b8707ab14a3554c7d7a9dc 0.56.5",
        "67520afcf329fb9d1bed7bf7a749897f65700a81 0.56.7",
        "c601620b07a76739e998a2788aab63c6624d6778 0.57.0",
        "09b986c5004f2c615aab33cabccc180e96ef2ff0 0.57.1",
        "9f93ee790fe58e1074b4094d4215bf4e334dd493 0.57.2",
        "ae18d29ec77443516e00ad3539aec8adbcc647f8 0.57.3",
        "5d93955c3a9047396eeb10088ea071428cca350a 0.58.0",
        "d8bb86832063e2dd26af8f974743980a897aee9a 0.58.1",
        "38f0d1446d868972a49c82bb2c51c360c169497b 0.58.2",
        "3843ad03ae6d4815443177c17ff0613ff282ccaa 0.58.3",
        "24cb3de3ba99aecf2b1c081207481791fca0cba4 0.58.6",
        "b256420ed6e655763e203c0b3d83a267b133f218 0.58.8",
        "8637070884864405d5d227a18f8e3dffa2383419 0.59.0",
        "8b54c0a9af32276b36b9d131518fd1a8f4ecb89b 0.59.1",
        "4f35be4404968b0f0fdaad80aee009bb69a77083 0.59.3",
        "61e155357e39756d72c3c69cac0f0bfe7e7e9ca7 0.59.5",
        "b381bbba63d2c9d2af6be3b0174315083d24f231 0.59.6",
        "daa9246648cf5b080e7d72b4d609023e59de97bd 0.59.7",
        "aa1b178598bff2b49ce419be924e8539a9fa19ae 0.59.9",
        "c9cac35bbba2104219288c251925f7a7be71c03d 0.61.1",
        "e05bfbb26605cec6b648ab69e142b73ad4cf45c8 0.61.2",
        "98162c382c4888fef1e1f999ae67f59a8d4f590f 0.61.3",
        "22fecd0b472c659ed00bbdc23fbc1b749145e410 0.62.0",
        "fa95dd7a4d02592ddebad57e03ef3dc0f227c363 0.62.1",
        "4ddf90b6b3e64995a5edc6ad876385bc8343fb7f 0.63.0",
        "c9d4c804e2f8235b4e994d385701cc9c2155a307 0.63.1",
        "3701d1473299cae579a4d513d7159a147dcac9f5 0.63.2",
        "125689ec288eea784f4d8e2394295283fb1edb38 master"
    );

    /**
     * Constructor.
     */
    CommitHashesText() {
        this(CommitHashesText::fetch);
    }

    /**
     * Constructor.
     *
     * @param source Text source, retried on its own before this class falls
     *  back to {@link CommitHashesText#FALLBACK}
     */
    CommitHashesText(final Text source) {
        super(new Synced(new Sticky(() -> CommitHashesText.safe(source))));
    }

    private static String safe(final Text source) throws Exception {
        String hashes;
        try {
            hashes = source.asString();
        } catch (final IOException ex) {
            Logger.warn(
                CommitHashesText.class,
                "Can't load hashes: %[exception]s",
                ex
            );
            hashes = CommitHashesText.FALLBACK;
        }
        return hashes;
    }

    @RetryOnFailure(delay = 1L, unit = TimeUnit.SECONDS)
    private static String fetch() throws Exception {
        return new TextOf(new URL("https://home.objectionary.com/tags.txt")).asString();
    }
}
