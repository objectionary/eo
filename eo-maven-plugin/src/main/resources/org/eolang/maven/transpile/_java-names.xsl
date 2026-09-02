<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="eo xs" id="_java-names" version="2.0">
  <!--
  How EO names become Java names: identifier escaping, class and package
  naming with the 250-character fingerprint cut, attribute naming, and the
  locator-to-class mapping. Extracted from "to-java.xsl" so that every sheet
  rendering Java (see #8137) derives a name the same way, since a declaration
  and a reference produced by two different sheets must never diverge.
  The "$eo:phi"/"$eo:alpha"/"$eo:cactoos" variables come from "_specials.xsl",
  which an importing sheet must bring in alongside (importing it from here
  trips Saxon's cycle check under the classpath resolver, whose sources
  carry no system identifiers).
  -->
  <!-- Unicode escape of a character Java forbids in an identifier -->
  <xsl:function name="eo:escape-char" as="xs:string">
    <xsl:param name="c" as="xs:string"/>
    <xsl:variable name="code" select="string-to-codepoints($c)[1]"/>
    <xsl:value-of select="concat('$u', string-join(for $w in (4096, 256, 16, 1) return substring('0123456789ABCDEF', ($code idiv $w) mod 16 + 1, 1), ''))"/>
  </xsl:function>
  <!-- Turn a name into a Java identifier, escaping every character Java forbids there -->
  <xsl:function name="eo:identifier" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:variable name="escaped">
      <xsl:analyze-string select="$n" regex="[^\p{{L}}\d_$]">
        <xsl:matching-substring>
          <xsl:value-of select="eo:escape-char(.)"/>
        </xsl:matching-substring>
        <xsl:non-matching-substring>
          <xsl:value-of select="."/>
        </xsl:non-matching-substring>
      </xsl:analyze-string>
    </xsl:variable>
    <xsl:value-of select="$escaped"/>
  </xsl:function>
  <!-- Turn a name into the body of a Java string literal, escaping the backslash and the quote -->
  <xsl:function name="eo:literal" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:value-of select="replace(replace($n, '\\', '\\\\'), '&quot;', '\\&quot;')"/>
  </xsl:function>
  <!-- Get clean escaped object name -->
  <xsl:function name="eo:clean" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:value-of select="concat('EO', eo:identifier(replace(replace(translate(translate(replace($n, '_', '__'), '-', '_'), '@', $eo:phi), $eo:alpha, '_'), '\$', '\$EO')))"/>
  </xsl:function>
  <!--
  A deterministic digit fingerprint of a name, computed purely from the name's own
  characters rather than from any surrounding XML node. Two over-long names sharing
  their first 240-odd characters still disambiguate, and the same name always
  fingerprints the same way regardless of which call site of "eo:class-name" asks,
  so a declaration and a reference to the same over-long name never diverge (#7254).
  Two polynomial hashes with different bases and different prime moduli, since a
  single weighted sum of the code points cancels out for names that differ in two
  positions only (#7633).
  -->
  <xsl:function name="eo:fingerprint" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:variable name="codes" select="string-to-codepoints($n)"/>
    <xsl:value-of select="concat('_', string(eo:polynomial($codes, 131, 1000000007, 0)), '_', string(eo:polynomial($codes, 137, 998244353, 0)))"/>
  </xsl:function>
  <!--
  A polynomial hash of the code points, folded left to right, so that the same
  characters in another order hash differently.
  -->
  <xsl:function name="eo:polynomial" as="xs:integer">
    <xsl:param name="codes" as="xs:integer*"/>
    <xsl:param name="base" as="xs:integer"/>
    <xsl:param name="modulo" as="xs:integer"/>
    <xsl:param name="acc" as="xs:integer"/>
    <xsl:choose>
      <xsl:when test="empty($codes)">
        <xsl:sequence select="$acc"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="eo:polynomial(subsequence($codes, 2), $base, $modulo, ($acc * $base + $codes[1]) mod $modulo)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  A cut prefix with any trailing dot dropped, so the digit-starting fingerprint
  appended after it lands inside an existing identifier segment instead of
  starting an illegal one of its own (#7254).
  -->
  <xsl:function name="eo:unbroken" as="xs:string">
    <xsl:param name="s" as="xs:string"/>
    <xsl:choose>
      <xsl:when test="ends-with($s, '.')">
        <xsl:value-of select="eo:unbroken(substring($s, 1, string-length($s) - 1))"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$s"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- Get class name for the object -->
  <xsl:function name="eo:class-name" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:variable name="parts" select="tokenize($n, '\.')"/>
    <xsl:variable name="package">
      <xsl:for-each select="$parts">
        <xsl:if test="position()!=last()">
          <xsl:value-of select="eo:clean(.)"/>
          <xsl:text>.</xsl:text>
        </xsl:if>
      </xsl:for-each>
    </xsl:variable>
    <xsl:variable name="class">
      <xsl:choose>
        <xsl:when test="$parts[last()]">
          <xsl:value-of select="$parts[last()]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$parts"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="pre" select="concat($package, eo:clean($class))"/>
    <xsl:choose>
      <xsl:when test="string-length($pre)&gt;250">
        <xsl:variable name="fingerprint" select="eo:fingerprint($n)"/>
        <xsl:value-of select="concat(eo:unbroken(substring($pre, 1, 250 - string-length($fingerprint))), $fingerprint)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$pre"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  Get the name of the JUnit class generated for the tests of an object. The
  mark goes in front of the class and not after it, because every name
  "eo:class-name" makes starts with "EO", so a name starting with "Test" is
  one it can never make. A suffix could be made: an object called "xTest"
  gives the same "EOxTest" the tests of an object called "x" used to give, and
  the two files then declare one class in one package (#7762).
  -->
  <xsl:function name="eo:test-class-name" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:variable name="full" select="eo:class-name($n)"/>
    <xsl:variable name="last" select="tokenize($full, '\.')[last()]"/>
    <xsl:value-of select="concat(substring($full, 1, string-length($full) - string-length($last)), 'Test', $last)"/>
  </xsl:function>
  <!-- Get clean escaped package segment, prefixed to never clash with an object class -->
  <xsl:function name="eo:clean-package" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:value-of select="concat('EO_', eo:identifier(replace(replace(translate(translate(replace($n, '_', '__'), '-', '_'), '@', $eo:phi), $eo:alpha, '_'), '\$', '\$EO')))"/>
  </xsl:function>
  <!-- Get Java package name for the EO package, one clean-package per segment -->
  <xsl:function name="eo:package-name" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:variable name="joined">
      <xsl:for-each select="tokenize($n, '\.')">
        <xsl:if test="position()!=1">
          <xsl:text>.</xsl:text>
        </xsl:if>
        <xsl:value-of select="eo:clean-package(.)"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:value-of select="$joined"/>
  </xsl:function>
  <!-- Get name for special attributes -->
  <xsl:function name="eo:attr-name" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:param name="wrap" as="xs:boolean"/>
    <xsl:variable name="alpha" select="starts-with($n, $eo:alpha)"/>
    <xsl:variable name="name">
      <xsl:choose>
        <xsl:when test="$n='@'">
          <xsl:value-of select="$eo:phi"/>
        </xsl:when>
        <xsl:when test="$alpha">
          <xsl:value-of select="substring-after($n, $eo:alpha)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$n"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$wrap">
        <xsl:choose>
          <xsl:when test="$alpha">
            <xsl:value-of select="$name"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:variable name="quoted">
              <xsl:text>"</xsl:text>
              <xsl:value-of select="eo:literal($name)"/>
              <xsl:text>"</xsl:text>
            </xsl:variable>
            <xsl:value-of select="$quoted"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$name"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!--
  Convert location to class name.

  Every glyph the locator can hold maps to a distinct one here, so that two
  locators cannot name one class - the way "_" maps to "__" and "-" to "_"
  since #7634. The dot separating the segments used to map to nothing at all,
  which made "x.a.bc" and "x.ab.c" one name and declared the same nested class
  twice in one file (#7761); it maps to "$" now, which Java accepts inside an
  identifier. A "$" the locator itself carries is escaped ahead of the join,
  so it cannot be read back as a separator.
  -->
  <xsl:function name="eo:loc-to-class">
    <xsl:param name="loc"/>
    <xsl:value-of select="concat('EO', eo:identifier(replace(translate(replace(string-join(tokenize(replace($loc, '\$', '\$u0024'), '\.'), '$'), '_', '__'), '-', '_'), $eo:cactoos, $eo:alpha)))"/>
  </xsl:function>
</xsl:stylesheet>
