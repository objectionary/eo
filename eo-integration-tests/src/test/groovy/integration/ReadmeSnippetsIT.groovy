/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package integration

import com.jcabi.manifests.Manifests
import com.yegor256.MayBeSlow
import com.yegor256.Mktmp
import com.yegor256.MktmpResolver
import com.yegor256.WeAreOnline
import com.yegor256.farea.Farea
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith

/**
 * Integration test that validates snippets from README.md file.
 * @since 0.56.3
 */
@ExtendWith(MktmpResolver.class)
final class ReadmeSnippetsIT {

  @Test
  @ExtendWith(WeAreOnline.class)
  @ExtendWith(MayBeSlow.class)
  void validatesReadmeSnippets(final @Mktmp Path temp) {
    def content = Files.readString(
      Paths.get("").toAbsolutePath().parent.resolve("README.md")
    )
    (content =~ /(?ms)```eo\s+(.*?)```/).each {
      match ->
        def snippet = match[1].trim()
        new Farea(temp).together(
          f -> {
            f.properties()
              .set("project.build.sourceEncoding", StandardCharsets.UTF_8.name())
              .set("project.reporting.outputEncoding", StandardCharsets.UTF_8.name())
            f.files()
              .file(String.format("src/main/eo/%s.eo", "snippet"))
              .write(
                String.format("%s\n", snippet).getBytes(StandardCharsets.UTF_8)
              )
            f.dependencies()
              .append(
                "org.eolang",
                "eo-runtime",
                System.getProperty(
                  "eo.version",
                  Manifests.read("EO-Version")
                )
              )
            f.build()
              .properties()
              .set("directory", "target")
            new EoMavenPlugin(f)
              .appended()
              .execution("compile")
              .phase("generate-sources")
              .goals("register", "compile", "transpile")
              .configuration()
              .set("failOnWarning", Boolean.FALSE.toString())
              .set("skipLinting", Boolean.TRUE.toString())
            f.build()
              .plugins()
              .append("org.codehaus.mojo", "exec-maven-plugin", "3.1.1")
              .execution("run")
              .phase("test")
              .goals("java")
              .configuration()
              .set("mainClass", "org.eolang.Main")
              .set("arguments", "app")
            f.exec("clean", "test")
          }
        )
    }
  }
}
