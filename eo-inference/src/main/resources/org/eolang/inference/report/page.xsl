<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<!--
A page of the inference report: the source of one file, with a mark on every
object in the colour of one of ten bands, and a list under the mark of what
the object answers in how many copies, one answer per bullet when there are
several. A click on a colour in the tally keeps only the lines with
marks of the chosen colours, and only those marks on them; a dashed rule shows
where lines are left out. The choice is shared with the index and the other
pages. A mark's list stays open while the pointer moves onto it, and a list
with selected text stays open until a click elsewhere or Escape, so the text
can be copied. An answer with no name of its own is shown by its shape, with
a line per attribute under its voids when it binds any, and
answers of the same shape are one answer, with a link to each line where one
is written and its locator as hover text on the link; the line a link leads to
stays lit until the next scroll, click or key press.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="page" version="3.0">
  <xsl:output method="html" version="5.0" encoding="UTF-8" indent="no" omit-xml-declaration="yes" include-content-type="no"/>
  <xsl:variable name="cap" select="8"/>
  <xsl:variable name="sites" select="12"/>
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
        <p class="tally" title="Click colours to show only their objects; click a chosen colour again to drop it">
          <span class="named" data-band="named">
            <xsl:value-of select="@named"/>
            <xsl:text> named</xsl:text>
          </span>
          <span class="gaps" data-band="gaps">
            <xsl:value-of select="@gaps"/>
            <xsl:text> one name, with gaps</xsl:text>
          </span>
          <span class="pair" data-band="pair">
            <xsl:value-of select="@pair"/>
            <xsl:text> two names</xsl:text>
          </span>
          <span class="mixed" data-band="mixed">
            <xsl:value-of select="@mixed"/>
            <xsl:text> two or more names, maybe with gaps</xsl:text>
          </span>
          <span class="lost" data-band="lost">
            <xsl:value-of select="@lost"/>
            <xsl:text> lost track</xsl:text>
          </span>
          <span class="atom" data-band="atom">
            <xsl:value-of select="@atom"/>
            <xsl:text> open void, filled by an atom</xsl:text>
          </span>
          <span class="unfilled" data-band="unfilled">
            <xsl:value-of select="@unfilled"/>
            <xsl:text> open void, filled by nobody</xsl:text>
          </span>
          <span class="error" data-band="error">
            <xsl:value-of select="@error"/>
            <xsl:text> only an error</xsl:text>
          </span>
          <span class="unreached" data-band="unreached">
            <xsl:value-of select="@unreached"/>
            <xsl:text> not reached</xsl:text>
          </span>
          <span class="fails" data-band="fails">
            <xsl:value-of select="@fails"/>
            <xsl:text> a read fails</xsl:text>
          </span>
        </p>
        <table class="src">
          <xsl:apply-templates select="line"/>
        </table>
        <xsl:call-template name="filter"/>
        <xsl:call-template name="hold"/>
        <xsl:call-template name="lit"/>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="line">
    <tr id="L{@n}">
      <td class="no">
        <xsl:value-of select="@n"/>
      </td>
      <td class="eo">
        <xsl:apply-templates select="bit"/>
      </td>
    </tr>
  </xsl:template>
  <xsl:template match="bit">
    <xsl:choose>
      <xsl:when test="@band">
        <span class="mark {@band}" data-band="{@band}">
          <xsl:value-of select="text"/>
          <span class="pop">
            <xsl:apply-templates select="told"/>
          </span>
        </span>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="told">
    <span class="said">
      <xsl:choose>
        <xsl:when test="@copies = 0">
          <xsl:text>not reached by any copy</xsl:text>
        </xsl:when>
        <xsl:when test="count(said) = 1">
          <xsl:apply-templates select="said"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:for-each select="said[position() &lt;= $cap]">
            <span class="item">
              <xsl:apply-templates select="."/>
            </span>
          </xsl:for-each>
          <xsl:if test="count(said) &gt; $cap">
            <span class="item">
              <xsl:text>and </xsl:text>
              <xsl:value-of select="count(said) - $cap"/>
              <xsl:text> more</xsl:text>
            </span>
          </xsl:if>
        </xsl:otherwise>
      </xsl:choose>
    </span>
  </xsl:template>
  <xsl:template match="said">
    <xsl:choose>
      <xsl:when test="@kind = 'name'">
        <code>
          <xsl:value-of select="."/>
        </code>
      </xsl:when>
      <xsl:when test="@kind = 'object'">
        <xsl:choose>
          <xsl:when test="contains(text(), '&#10;')">
            <code class="tree">
              <xsl:value-of select="text()"/>
            </code>
            <xsl:text>written at </xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <code>
              <xsl:value-of select="text()"/>
            </code>
            <xsl:text>, written at </xsl:text>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:for-each select="at[position() &lt;= $sites]">
          <xsl:choose>
            <xsl:when test="position() = 1"/>
            <xsl:when test="position() = last() and count(../at) &lt;= $sites">
              <xsl:text> and </xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:text>, </xsl:text>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:value-of select="@lead"/>
          <a href="{@href}" title="{@loc}">
            <xsl:value-of select="."/>
          </a>
        </xsl:for-each>
        <xsl:if test="count(at) &gt; $sites">
          <xsl:text> and </xsl:text>
          <xsl:value-of select="count(at) - $sites"/>
          <xsl:text> more</xsl:text>
        </xsl:if>
      </xsl:when>
      <xsl:when test="@kind = 'fails'">
        <xsl:text>a read that fails, </xsl:text>
        <code>
          <xsl:value-of select="."/>
        </code>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:choose>
      <xsl:when test="../@copies = 1"/>
      <xsl:when test="@n = ../@copies">
        <xsl:text> (all </xsl:text>
        <xsl:value-of select="../@copies"/>
        <xsl:text> copies)</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text> (</xsl:text>
        <xsl:value-of select="@n"/>
        <xsl:text> of </xsl:text>
        <xsl:value-of select="../@copies"/>
        <xsl:text> copies)</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="style">
    <style>
      body { font: 13px/1.5 ui-monospace, Menlo, Consolas, monospace; margin: 2em; color: #1a1a1a; background: #fff; }
      h1 { font-size: 15px; font-weight: 600; margin: 0 0 .8em; }
      a { color: #0645ad; }
      .tally span { margin-right: 1.4em; padding: .1em .4em; border-radius: 3px; display: inline-block; margin-bottom: .3em; }
      .tally .named { background: #d7f0d7; }
      .tally .gaps { background: #eef3c2; }
      .tally .pair { background: #d3eff2; }
      .tally .mixed { background: #d9e5fb; }
      .tally .lost { background: #fbeecc; }
      .tally .atom { background: #e6dcf5; }
      .tally .unfilled { background: #dfe6e9; }
      .tally .error { background: #efe2dc; }
      .tally .unreached { background: #eeeeee; }
      .tally .fails { background: #f7d4d4; }
      .tally span[data-band] { cursor: pointer; user-select: none; }
      .tally span.on { box-shadow: 0 0 0 2px #1a1a1a; }
      table.src { border-collapse: collapse; }
      td.no { text-align: right; padding-right: 1em; color: #999; user-select: none; vertical-align: top; }
      td.eo { white-space: pre; }
      tr.hidden { display: none; }
      table.src td { transition: background-color .6s; }
      tr.lit td { background: #fff3b0; transition: none; }
      tr.cut td { border-top: 1px dashed #ccc; }
      .mark { position: relative; box-shadow: inset 0 -2px 0 currentColor; cursor: help; }
      .mark.named { color: #2e7d32; }
      .mark.gaps { color: #7c8c00; }
      .mark.pair { color: #00838f; }
      .mark.mixed { color: #1e5bc6; }
      .mark.lost { color: #b26a00; }
      .mark.atom { color: #6a3fb5; }
      .mark.unfilled { color: #546e7a; }
      .mark.error { color: #795548; }
      .mark.unreached { color: #9e9e9e; }
      .mark.fails { color: #c62828; }
      .pop { display: none; position: absolute; left: 0; top: 100%; margin-top: .3em; z-index: 9; white-space: normal; width: 26em; padding: .6em .8em; background: #fffdf5; color: #1a1a1a; border: 1px solid #ccc; box-shadow: 0 2px 6px rgba(0,0,0,.15); }
      .pop::before { content: ""; position: absolute; left: -1px; right: -1px; bottom: 100%; height: calc(.3em + 1px); }
      .mark:hover .pop, .mark.held .pop { display: block; }
      .mark.off { color: inherit; box-shadow: none; cursor: auto; }
      .mark.off:hover .pop { display: none; }
      .said { display: block; }
      .said code.tree { display: block; white-space: pre-wrap; }
      .item { display: list-item; list-style: disc; margin-left: 1.2em; }
      code { background: #f2f2f2; padding: 0 .2em; }
    </style>
  </xsl:template>
  <xsl:template name="filter">
    <script>
      (function () {
        var chips = Array.from(document.querySelectorAll('.tally span[data-band]'));
        var marks = Array.from(document.querySelectorAll('.mark'));
        var lines = Array.from(document.querySelectorAll('table.src tr'));
        function shown() {
          var picked = chips.filter(function (chip) { return chip.classList.contains('on'); }).map(function (chip) { return chip.dataset.band; });
          marks.forEach(function (mark) {
            mark.classList.toggle('off', picked.length > 0 ? picked.indexOf(mark.dataset.band) === -1 : false);
          });
          var previous = -1;
          lines.forEach(function (line, at) {
            var kept = picked.length === 0 || line.querySelector('.mark:not(.off)') !== null;
            line.classList.toggle('hidden', !kept);
            line.classList.toggle('cut', kept &amp;&amp; previous >= 0 &amp;&amp; at > previous + 1);
            if (kept) { previous = at; }
          });
          try { localStorage.setItem('bands', picked.join(' ')); } catch (ignored) {}
        }
        var saved = '';
        try { saved = localStorage.getItem('bands') || ''; } catch (ignored) {}
        chips.forEach(function (chip) {
          chip.classList.toggle('on', saved.split(' ').indexOf(chip.dataset.band) !== -1);
          chip.addEventListener('click', function () { chip.classList.toggle('on'); shown(); });
        });
        shown();
      })();
    </script>
  </xsl:template>
  <xsl:template name="hold">
    <script>
      (function () {
        var held = null;
        function release() {
          if (held !== null) { held.classList.remove('held'); held = null; }
        }
        document.addEventListener('mousedown', function (event) {
          var pop = event.target.closest('.pop');
          release();
          if (pop !== null) { held = pop.parentNode; held.classList.add('held'); }
        });
        document.addEventListener('mouseup', function () {
          if (held !== null &amp;&amp; String(window.getSelection()) === '') { release(); }
        });
        document.addEventListener('keydown', function (event) {
          if (event.key === 'Escape') { release(); }
        });
      })();
    </script>
  </xsl:template>
  <xsl:template name="lit">
    <script>
      (function () {
        var lit = null;
        function dim() {
          if (lit !== null) { lit.classList.remove('lit'); lit = null; }
        }
        function light() {
          dim();
          lit = location.hash === '' ? null : document.getElementById(location.hash.slice(1));
          if (lit !== null) { lit.classList.add('lit'); }
        }
        ['wheel', 'touchmove', 'keydown', 'mousedown'].forEach(function (name) {
          document.addEventListener(name, dim, { passive: true });
        });
        document.addEventListener('click', function (event) {
          var link = event.target.closest('a');
          if (link !== null &amp;&amp; link.hash === location.hash &amp;&amp; link.pathname === location.pathname) { light(); }
        });
        window.addEventListener('hashchange', light);
        light();
      })();
    </script>
  </xsl:template>
</xsl:stylesheet>
