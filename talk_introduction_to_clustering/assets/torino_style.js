//addResourcePath("assets", "assets/");

// Higher order helpers
function add(el, html_tag0, class0, fun_pend) {
  new_el = document.createElement(html_tag0);
  new_el.classList.add(class0);
  el[fun_pend](new_el);
}

function add_logo(el, class0, fpath) {
  var para = document.createElement("img");
  para.classList.add(class0);
  para.src = fpath;
  el.prepend(para);
}

// Normal slides
// header
var add_header = el => add(el, "div", "header-banner", "prepend");
var add_header_underline = el => add(el, "div", "header-underline", "append");
// footer
var add_footer_one   = el => add(el, "div", "footer-one-banner", "append");
var add_footer_two   = el => add(el, "div", "footer-two-banner", "append");
var add_footer_three = el => add(el, "div", "footer-three-banner", "append");
function add_footer(el) {
  add_footer_one(el);
  add_footer_two(el);
  add_footer_three(el);
}
// image
var add_mini_logo = el => add_logo(el, "img-logo", "assets/UOM.png");

// Title slides
var add_big_logo = el => add_logo(el, "title-logo", "assets/UOM.png");
var add_title_b1 = el => add(el, "div", "title-header-1", "prepend");
var add_title_b2 = el => add(el, "div", "title-header-2", "prepend");
var add_title_b3 = el => add(el, "div", "title-header-3", "prepend");
var add_title_b4 = el => add(el, "div", "title-header-4", "prepend");
function add_title_banner(el) {
  add_title_b4(el);
  add_title_b3(el);
  add_title_b2(el);
  add_title_b1(el);
}

function add_text(el, class0, text0) {
  var para = document.createElement("div");
  para.classList.add(class0);
  para.innerText = text0;
  el.append(para);
}
// Others
var change_bg_color = (el, color) => {el.style = "background-color:" + color};

/*========================================================================*/
// Main code
/*========================================================================*/
var slides = document.getElementsByTagName("slide");
var headers = document.getElementsByTagName("h2");

// Handle title slide
add_title_banner(slides[0]);
add_big_logo(slides[0]);
change_bg_color(slides[0], 'white');
add_text(slides[0], "my-title", "How to make Ioslides look like Beamer");
add_text(slides[0], "my-sub-title", "Jackson Kwok and Kate Saunders");
add_text(slides[0], "my-info", "December 2018");

// Handle normal slide
for (i = 1; i < slides.length; i++) {
  add_header(slides[i]);
  add_footer(slides[i]);
  change_bg_color(slides[i], 'white');
}
for (i = 1; i < headers.length; i++) {
  if (headers[i].innerText !== "") {
    add_mini_logo(slides[i]);
    add_header_underline(headers[i]);
  }
}
