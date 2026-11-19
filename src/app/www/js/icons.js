// icons.js ― 自作の線画アイコン（32×32、線 1.8、端と角は丸）
(function () {
  'use strict';
  var G = '#CDE8D6', W = '#fff', ADD = '#2E7D32', DEL = '#C00000', Y = '#FFE9A8';
  var sheet = '<rect x="4" y="6" width="24" height="20" fill="' + W + '"/><rect x="4" y="6" width="24" height="6" fill="' + G + '"/><path d="M4 12h24M4 19h24M12 6v20M20 6v20"/>';
  var paper = '<path d="M8 4h12l6 6v18H8z" fill="' + W + '"/><path d="M20 4v6h6"/>';
  var axes = '<path d="M5 5v22h23"/>';
  var P = {
    // CSV を開く：小さめの表と、右下の白い丸の中の下向き矢印（矢印が表の線と重ならないようにする）
    csv: '<rect x="3" y="4" width="20" height="16" fill="' + W + '"/><rect x="3" y="4" width="20" height="5" fill="' + G + '"/><path d="M3 9h20M3 14.500h20M9.700 4v16M16.300 4v16"/>' +
      '<circle cx="24" cy="23.500" r="6.500" fill="' + W + '" stroke="' + ADD + '"/><path d="M24 19.500v7M21 23.800l3 3 3-3" stroke="' + ADD + '" stroke-width="2.2"/>',
    open: '<path d="M4 9h8l3 3h13v14H4z" fill="' + Y + '"/><path d="M4 26l4-10h23l-3 10" fill="' + W + '"/>',
    save: '<path d="M5 5h18l4 4v18H5z" fill="' + G + '"/><rect x="10" y="5" width="11" height="7" fill="' + W + '"/><rect x="9" y="17" width="14" height="10" fill="' + W + '"/>',
    saveas: '<path d="M5 5h18l4 4v18H5z" fill="' + G + '"/><rect x="10" y="5" width="11" height="7" fill="' + W + '"/><path d="M14 25l2-6 9-9 4 4-9 9z" fill="' + W + '"/>',
    undo: '<path d="M11 6l-6 6 6 6"/><path d="M5 12h13a8 8 0 010 16h-6"/>',
    redo: '<path d="M21 6l6 6-6 6"/><path d="M27 12H14a8 8 0 000 16h6"/>',
    add: sheet + '<circle cx="24" cy="23" r="6.5" fill="' + W + '" stroke="' + ADD + '"/><path d="M24 19.5v7M20.5 23h7" stroke="' + ADD + '" stroke-width="2.2"/>',
    dup: '<rect x="4" y="4" width="17" height="17" fill="' + W + '"/><rect x="11" y="11" width="17" height="17" fill="' + G + '"/>',
    del: sheet + '<path d="M19 18l9 9M28 18l-9 9" stroke="' + DEL + '" stroke-width="2.4"/>',
    swap: '<rect x="4" y="5" width="9" height="22" fill="' + G + '"/><rect x="4" y="5" width="24" height="7" fill="' + W + '"/><path d="M17 24h9M22 20l4 4-4 4M24 16V15"/>' ,
    change: '<path d="M6 12a10 10 0 0118-4M26 20a10 10 0 01-18 4"/><path d="M24 3v6h-6M8 29v-6h6"/>',
    filter: '<path d="M4 6h24l-9 11v9l-6 2V17z" fill="' + G + '"/>',
    data: sheet,
    vars: '<rect x="4" y="5" width="24" height="22" fill="' + W + '"/><path d="M4 12h24M4 19h24"/><rect x="7" y="7.5" width="5" height="2.5" fill="' + G + '"/><rect x="7" y="14.5" width="5" height="2.5" fill="' + G + '"/><rect x="7" y="21.5" width="5" height="2.5" fill="' + G + '"/>',
    k_stack100: '<rect x="5" y="6" width="9" height="6" fill="#5F7D6C"/><rect x="14" y="6" width="9" height="6" fill="' + G + '"/><rect x="23" y="6" width="5" height="6" fill="' + W + '"/><rect x="5" y="16" width="5" height="6" fill="#5F7D6C"/><rect x="10" y="16" width="8" height="6" fill="' + G + '"/><rect x="18" y="16" width="10" height="6" fill="' + W + '"/><path d="M5 3v26"/>',
    k_stack: '<rect x="6" y="14" width="6" height="13" fill="#5F7D6C"/><rect x="6" y="8" width="6" height="6" fill="' + G + '"/><rect x="16" y="19" width="6" height="8" fill="#5F7D6C"/><rect x="16" y="11" width="6" height="8" fill="' + G + '"/><path d="M3 27h26"/>',
    k_group: '<rect x="5" y="12" width="5" height="15" fill="#5F7D6C"/><rect x="10" y="18" width="5" height="9" fill="' + G + '"/><rect x="18" y="8" width="5" height="19" fill="#5F7D6C"/><rect x="23" y="15" width="5" height="12" fill="' + G + '"/><path d="M3 27h26"/>',
    k_heat: '<rect x="5" y="5" width="22" height="22" fill="' + W + '"/><rect x="5" y="5" width="11" height="11" fill="#5F7D6C"/><rect x="16" y="16" width="11" height="11" fill="' + G + '"/><path d="M16 5v22M5 16h22"/>',
    k_bar: '<rect x="6" y="9" width="5" height="18" fill="' + G + '"/><rect x="13.5" y="15" width="5" height="12" fill="' + G + '"/><rect x="21" y="20" width="5" height="7" fill="' + G + '"/><path d="M3 27h26"/>',
    k_box: '<path d="M10 4v5M10 21v6M7 4h6M7 27h6M22 8v5M22 23v4M19 8h6M19 27h6"/><rect x="6" y="9" width="8" height="12" fill="' + G + '"/><path d="M6 14h8"/><rect x="18" y="13" width="8" height="10" fill="' + G + '"/><path d="M18 19h8"/>',
    k_pie: '<circle cx="16" cy="16" r="11" fill="' + W + '"/><path d="M16 16V5a11 11 0 0110.400 14.500z" fill="#5F7D6C"/><path d="M16 16l10.400 3.500A11 11 0 0110 25.200z" fill="' + G + '"/>',
    k_mean: axes + '<path d="M12 8v12M10 8h4M10 20h4M21 12v10M19 12h4M19 22h4"/><circle cx="12" cy="14" r="2.6" fill="' + G + '"/><circle cx="21" cy="17" r="2.6" fill="' + G + '"/>',
    k_line: axes + '<path d="M8 21l5-6 5 3 4-8 5 2" stroke-width="2"/><circle cx="13" cy="15" r="1.6" fill="#1E5C3A"/><circle cx="18" cy="18" r="1.6" fill="#1E5C3A"/><circle cx="22" cy="10" r="1.6" fill="#1E5C3A"/>',
    k_hist: axes + '<rect x="8" y="17" width="4" height="9" fill="' + G + '"/><rect x="12" y="10" width="4" height="16" fill="' + G + '"/><rect x="16" y="7" width="4" height="19" fill="' + G + '"/><rect x="20" y="13" width="4" height="13" fill="' + G + '"/><rect x="24" y="20" width="3" height="6" fill="' + G + '"/>',
    k_map: '<rect x="4" y="4" width="24" height="24" fill="#fff"/><path d="M16 5v22M5 16h22" stroke-dasharray="2 2"/><circle cx="10" cy="11" r="2" fill="' + G + '"/><circle cx="21" cy="21" r="2" fill="' + G + '"/><path d="M21 8l2.5 4h-5z" fill="#fff"/><path d="M10 19l2.5 4h-5z" fill="#fff"/>',
    k_cloud: '<rect x="4" y="6" width="24" height="20" fill="#fff"/><path d="M8 17h10" stroke-width="3.2"/><path d="M9 11h6M18 11h5M20 17h4M7 22h6M16 22h8"/>',
    k_wbar: '<path d="M7 4v24"/><rect x="7" y="7" width="18" height="4" fill="' + G + '"/><rect x="7" y="14" width="13" height="4" fill="' + G + '"/><rect x="7" y="21" width="8" height="4" fill="' + G + '"/>',
    k_net: '<path d="M9 10l8 6M17 16l7-8M17 16l-6 9M17 16l8 7" stroke="#7FA08F"/><circle cx="9" cy="10" r="3" fill="' + G + '"/><circle cx="17" cy="16" r="4" fill="' + G + '"/><circle cx="24" cy="8" r="2.5" fill="#fff"/><circle cx="11" cy="25" r="2.5" fill="#fff"/><circle cx="25" cy="23" r="3" fill="' + G + '"/>',
    k_scatter: axes + '<circle cx="11" cy="21" r="1.6" fill="' + G + '"/><circle cx="15" cy="16" r="1.6" fill="' + G + '"/><circle cx="19" cy="18" r="1.6" fill="' + G + '"/><circle cx="23" cy="10" r="1.6" fill="' + G + '"/>',
    horiz: '<path d="M6 4v24"/><rect x="6" y="7" width="18" height="6" fill="' + G + '"/><rect x="6" y="18" width="11" height="6" fill="' + G + '"/>',
    vert: '<path d="M4 26h24"/><rect x="7" y="8" width="6" height="18" fill="' + G + '"/><rect x="18" y="15" width="6" height="11" fill="' + G + '"/>',
    labels: '<rect x="5" y="14" width="22" height="9" fill="' + G + '"/><path d="M11 8h2v0M13 5v6M17 5.5a2.5 2.5 0 014 2c0 2-4 3.500-4 3.500h4.5"/>',
    grid: '<path d="M5 5v22h22"/><path d="M11 5v22M17 5v22M23 5v22" stroke-dasharray="2 3"/>',
    nlabel: '<path d="M5 22V10l8 12V10"/><path d="M18 13h9M18 19h9"/>',
    transparent: '<rect x="5" y="5" width="22" height="22" fill="' + W + '"/><rect x="5" y="5" width="7.300" height="7.300" fill="' + G + '" stroke="none"/><rect x="19.700" y="5" width="7.300" height="7.300" fill="' + G + '" stroke="none"/><rect x="12.300" y="12.300" width="7.400" height="7.400" fill="' + G + '" stroke="none"/><rect x="5" y="19.700" width="7.300" height="7.300" fill="' + G + '" stroke="none"/><rect x="19.700" y="19.700" width="7.300" height="7.300" fill="' + G + '" stroke="none"/><rect x="5" y="5" width="22" height="22"/>',
    fup: '<path d="M4 26l7-19 7 19M6.500 20h9"/><path d="M24 15V6M20 10l4-4 4 4" stroke="' + ADD + '" stroke-width="2.2"/>',
    fdown: '<path d="M4 26l7-19 7 19M6.500 20h9"/><path d="M24 6v9M20 11l4 4 4-4" stroke="' + ADD + '" stroke-width="2.2"/>',
    png: '<rect x="4" y="6" width="24" height="20" fill="' + W + '"/><path d="M4 22l7-7 5 5 4-4 8 8" fill="' + G + '"/><circle cx="21" cy="11" r="2" fill="' + Y + '"/>',
    svg: '<rect x="4" y="6" width="24" height="20" fill="' + W + '"/><path d="M8 21c3-10 6-10 8-4s5 4 8-6"/><circle cx="8" cy="21" r="1.800" fill="' + G + '"/><circle cx="24" cy="11" r="1.800" fill="' + G + '"/>',
    copy: '<rect x="10" y="4" width="16" height="19" fill="' + W + '"/><rect x="5" y="9" width="16" height="19" fill="' + G + '"/>',
    report: paper + '<path d="M12 14h10M12 18h10M12 22h6"/>',
    txt: paper + '<path d="M12 15h8M16 15v8"/>',
    rcode: paper + '<path d="M12 24V13h4.500a3 3 0 010 6H12M15.500 19l3.500 5" stroke-width="2"/>',
    link: '<path d="M13 19l6-6"/><path d="M14 9l2-2a5 5 0 017 7l-2 2M18 23l-2 2a5 5 0 01-7-7l2-2"/>',
    docx: paper + '<path d="M11 13l2 9 3-7 3 7 2-9"/>',
    html: paper + '<rect x="11" y="13" width="6" height="5" fill="' + G + '"/><path d="M19 14h4M19 17h4M11 22h12"/>',
    refresh: '<path d="M26 16a10 10 0 11-3-7"/><path d="M26 4v6h-6"/>',
    table: sheet,
    pct: '<circle cx="9" cy="9" r="3.500" fill="' + G + '"/><circle cx="23" cy="23" r="3.500" fill="' + G + '"/><path d="M25 5L7 27"/>',
    resid: '<rect x="4" y="6" width="24" height="20" fill="' + W + '"/><path d="M4 16h24M16 6v20"/><path d="M7 13l3-4 3 4z" fill="' + ADD + '" stroke="' + ADD + '"/><path d="M19 19l3 4 3-4z" fill="' + W + '"/>',
    zoom: '<circle cx="14" cy="14" r="8" fill="' + W + '"/><path d="M20 20l8 8M10 14h8M14 10v8"/>',
    report: '<path d="M6 7h20v13H14l-6 5v-5H6z" fill="' + W + '"/><path d="M16 10v5"/><circle cx="16" cy="17.8" r="1" fill="#1E5C3A"/>',
    info: '<circle cx="16" cy="16" r="11" fill="' + W + '"/><path d="M16 14v9"/><circle cx="16" cy="9.500" r="1" fill="#1E5C3A"/>',
    rename: '<path d="M5 24h22" /><path d="M9 19l2-6 10-9 4 4-10 9z" fill="' + W + '"/>',
    holm: '<rect x="4" y="5" width="24" height="22" fill="' + W + '"/><path d="M4 12h24M4 19h24M18 5v22"/><path d="M21 15.500h4M21 22.500h4M21 8.500h4"/>',
    font: '<path d="M6 26l8-21 8 21M9 19h10"/><path d="M23 26h5"/>',
    up: '<rect x="5" y="19" width="22" height="7" fill="' + W + '"/><rect x="5" y="6" width="22" height="7" fill="' + G + '"/><path d="M16 17V9M12.500 12l3.500-3.500 3.500 3.500" stroke="' + ADD + '" stroke-width="2.2"/>',
    down: '<rect x="5" y="6" width="22" height="7" fill="' + W + '"/><rect x="5" y="19" width="22" height="7" fill="' + G + '"/><path d="M16 15v8M12.500 20l3.500 3.500 3.500-3.500" stroke="' + ADD + '" stroke-width="2.2"/>',
    fold: '<rect x="5" y="5" width="22" height="6" fill="' + G + '"/><rect x="5" y="13" width="22" height="6" fill="' + G + '"/><rect x="5" y="21" width="22" height="6" fill="' + G + '"/>',
    unfold: '<rect x="5" y="4" width="22" height="5" fill="' + G + '"/><rect x="5" y="9" width="22" height="9" fill="' + W + '"/><rect x="5" y="22" width="22" height="5" fill="' + G + '"/><path d="M9 13h14"/>',
    exportall: '<path d="M4 9h8l3 3h13v14H4z" fill="' + Y + '"/><path d="M16 15v9M12 20l4 4 4-4" stroke="' + ADD + '" stroke-width="2.2"/>',
    palette: '<rect x="4" y="8" width="6" height="16" fill="#5F7D6C"/><rect x="10" y="8" width="6" height="16" fill="' + G + '"/><rect x="16" y="8" width="6" height="16" fill="' + Y + '"/><rect x="22" y="8" width="6" height="16" fill="' + W + '"/>',
    figadd: axes + '<rect x="9" y="15" width="5" height="12" fill="' + G + '"/><rect x="16" y="10" width="5" height="17" fill="' + G + '"/><path d="M25 4v8M21 8h8" stroke="' + ADD + '" stroke-width="2.2"/>',
    figdel: axes + '<rect x="9" y="15" width="5" height="12" fill="' + G + '"/><rect x="16" y="10" width="5" height="17" fill="' + G + '"/><path d="M22 4l6 6M28 4l-6 6" stroke="' + DEL + '" stroke-width="2.2"/>',
    checkall: '<rect x="5" y="5" width="22" height="22" fill="' + W + '"/><path d="M10 16l4 4 8-9" stroke="' + ADD + '" stroke-width="2.4"/>',
    uncheck: '<rect x="5" y="5" width="22" height="22" fill="' + W + '"/>',
    excel: sheet + '<path d="M19 17l8 10M27 17l-8 10" stroke="' + ADD + '" stroke-width="2.2"/>',
    t_grid: '<rect x="4" y="6" width="24" height="20" fill="' + W + '"/><rect x="4" y="6" width="24" height="6" fill="' + G + '"/><path d="M4 12h24M4 19h24M12 6v20M20 6v20"/>',
    t_three: '<path d="M4 7h24M4 25h24" stroke-width="2.4"/><path d="M4 13h24" stroke-width="1.2"/><path d="M7 18h4M14 18h4M21 18h4M7 22h4M14 22h4M21 22h4M7 10h4M14 10h4M21 10h4" stroke-width="1.2" stroke="#5F7D6C"/>',
    t_stripe: '<rect x="4" y="12" width="24" height="5" fill="' + G + '" stroke="none"/><rect x="4" y="22" width="24" height="5" fill="' + G + '" stroke="none"/><path d="M4 11h24"/><path d="M7 8h4M14 8h4M21 8h4M7 14.500h4M14 14.500h4M21 14.500h4M7 19.500h4M14 19.500h4M21 19.500h4M7 24.500h4M14 24.500h4M21 24.500h4" stroke-width="1.2" stroke="#5F7D6C"/>',
    t_plain: '<path d="M4 12h24"/><path d="M7 8h4M14 8h4M21 8h4M7 16h4M14 16h4M21 16h4M7 20.500h4M14 20.500h4M21 20.500h4M7 25h4M14 25h4M21 25h4" stroke-width="1.2" stroke="#5F7D6C"/>',
    t_head: '<rect x="4" y="5" width="24" height="7" fill="#1E5C3A"/><path d="M4 18h24M4 24h24" stroke-width="1"/><path d="M7 15h4M14 15h4M21 15h4M7 21h4M14 21h4M21 21h4M7 27h4M14 27h4M21 27h4" stroke-width="1.2" stroke="#5F7D6C"/>',
    colorbar: '<rect x="5" y="6" width="15" height="20" fill="' + W + '"/><rect x="5" y="6" width="7.500" height="10" fill="#5F7D6C"/><rect x="12.500" y="16" width="7.500" height="10" fill="' + G + '"/><rect x="23" y="6" width="4" height="7" fill="#5F7D6C"/><rect x="23" y="13" width="4" height="7" fill="' + G + '"/><rect x="23" y="20" width="4" height="6" fill="' + W + '"/>'
  };
  window.icon = function (name, size) {
    size = size || 30;
    return '<svg class="md-ic" viewBox="0 0 32 32" width="' + size + '" height="' + size + '" fill="none" stroke="#1E5C3A" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">' + (P[name] || '') + '</svg>';
  };
})();
