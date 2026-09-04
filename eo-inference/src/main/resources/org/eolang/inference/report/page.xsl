<?xml version="1.0" encoding="UTF-8"?>
<!--
SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="page" version="2.0">
  <xsl:output method="html" version="5.0" encoding="UTF-8" indent="no" omit-xml-declaration="yes"/>
  <xsl:template match="/page">
    <html lang="en">
      <head>
        <meta charset="utf-8"/>
        <title>
          <xsl:value-of select="@file"/>
        </title>
        <xsl:call-template name="style"/>
      </head>
      <body>
        <h1>
          <a href="{@root}index.html">all files</a>
          <xsl:text> / </xsl:text>
          <xsl:value-of select="@file"/>
        </h1>
        <p class="tally">
          <span class="named">
            <xsl:value-of select="@named"/>
            <xsl:text> named</xsl:text>
          </span>
          <span class="rooted">
            <xsl:value-of select="@rooted"/>
            <xsl:text> rooted at a void</xsl:text>
          </span>
          <span class="atom">
            <xsl:value-of select="@atom"/>
            <xsl:text> filled by an atom</xsl:text>
          </span>
          <span class="unfilled">
            <xsl:value-of select="@unfilled"/>
            <xsl:text> filled by nobody</xsl:text>
          </span>
          <span class="blank">
            <xsl:value-of select="@blank"/>
            <xsl:text> nothing known</xsl:text>
          </span>
        </p>
        <table class="src">
          <xsl:apply-templates select="line"/>
        </table>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="line">
    <tr>
      <td class="no">
        <xsl:value-of select="@n"/>
      </td>
      <td class="eo">
        <xsl:apply-templates select="bit"/>
      </td>
    </tr>
  </xsl:template>
  <xsl:template match="bit[not(@band)]">
    <xsl:value-of select="."/>
  </xsl:template>
  <xsl:template match="bit[@band]">
    <span class="mark {@band}">
      <xsl:value-of select="text"/>
      <span class="pop">
        <xsl:apply-templates select="told"/>
      </span>
    </span>
  </xsl:template>
  <xsl:template match="told">
    <span class="said">
      <b>
        <xsl:value-of select="@label"/>
      </b>
      <xsl:text>: </xsl:text>
      <xsl:choose>
        <xsl:when test="@void = 'true'">
          <xsl:text>void</xsl:text>
        </xsl:when>
        <xsl:when test="@band = 'blank'">
          <xsl:text>unknown</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <code>
            <xsl:value-of select="@where"/>
          </code>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:if test="@band = 'atom'">
        <xsl:text>, filled by an atom</xsl:text>
      </xsl:if>
      <xsl:if test="@band = 'unfilled'">
        <xsl:text>, filled by nobody</xsl:text>
      </xsl:if>
      <xsl:apply-templates select="seen"/>
    </span>
  </xsl:template>
  <xsl:template match="seen">
    <xsl:text>, seen </xsl:text>
    <xsl:for-each select="*">
      <xsl:if test="position() &gt; 1">
        <xsl:text>, </xsl:text>
      </xsl:if>
      <xsl:apply-templates select="."/>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="seen/ref">
    <code>
      <xsl:value-of select="@loc"/>
    </code>
  </xsl:template>
  <xsl:template match="seen/var">
    <xsl:text>void </xsl:text>
    <code>
      <xsl:value-of select="@id"/>
    </code>
  </xsl:template>
  <xsl:template match="seen/data">
    <xsl:text>a datum</xsl:text>
  </xsl:template>
  <xsl:template match="seen/unknown">
    <xsl:text>too many things to name</xsl:text>
  </xsl:template>
  <xsl:template name="style">
    <style>
      body { font: 13px/1.5 ui-monospace, Menlo, Consolas, monospace; margin: 2em; color: #1a1a1a; background: #fff; }
      h1 { font-size: 15px; font-weight: 600; margin: 0 0 .8em; }
      a { color: #0645ad; }
      .tally span { margin-right: 1.4em; padding: .1em .4em; border-radius: 3px; }
      .tally .named { background: #d7f0d7; }
      .tally .rooted { background: #fbeecc; }
      .tally .atom { background: #e6dcf5; }
      .tally .unfilled { background: #dfe6e9; }
      .tally .blank { background: #f7d4d4; }
      table.src { border-collapse: collapse; }
      td.no { text-align: right; padding-right: 1em; color: #999; user-select: none; vertical-align: top; }
      td.eo { white-space: pre; }
      .mark { position: relative; box-shadow: inset 0 -2px 0 currentColor; cursor: help; }
      .mark.named { color: #2e7d32; }
      .mark.rooted { color: #b26a00; }
      .mark.atom { color: #6a3fb5; }
      .mark.unfilled { color: #546e7a; }
      .mark.blank { color: #c62828; }
      .pop { display: none; position: absolute; left: 0; top: 1.6em; z-index: 9; white-space: normal; width: 26em; padding: .6em .8em; background: #fffdf5; color: #1a1a1a; border: 1px solid #ccc; box-shadow: 0 2px 6px rgba(0,0,0,.15); }
      .mark:hover .pop { display: block; }
      .said { display: block; }
      code { background: #f2f2f2; padding: 0 .2em; }
    </style>
  </xsl:template>
</xsl:stylesheet>
