/**
 * SPDX-FileCopyrightText: Copyright (c) 2012-2026 Yegor Bugayenko
 * SPDX-License-Identifier: MIT
 */

/*globals $:false, window:false */

$(document).ready(
    function () {
        $('.menu-button').click(
            function() {
                if ($('#here-goes-menu').attr('class') === 'menu-off') {
                    $('#menu').clone().attr('class', 'mobile-menu').appendTo('#here-goes-menu');
                    $('#here-goes-menu').attr('class', 'menu-on');
                } else {
                    $('.mobile-menu').hide();
                    $('#here-goes-menu').attr('class', 'menu-off');
                }
            }
        );
    }
);
