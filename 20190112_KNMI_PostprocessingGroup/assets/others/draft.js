add_header = function(single_slide) {
  var para = document.createElement("div");
  para.classList.add("header-banner");
  single_slide.prepend(para);
};
add_logo = function(single_slide) {
  var para = document.createElement("img");
  para.classList.add("img-logo");
  para.src = "temp2/UOM.png";
  single_slide.prepend(para);
};
add_header_underline = function(header_dom) {
  var para = document.createElement("div");
  para.classList.add("header-underline");
  header_dom.append(para);
};
add_footer = function(single_slide) {
  var para_1 = document.createElement("div");
  var para_2 = document.createElement("div");
  var para_3 = document.createElement("div");
  para_1.classList.add("footer-one-banner");
  para_2.classList.add("footer-two-banner");
  para_3.classList.add("footer-three-banner");
  single_slide.append(para_1);
  single_slide.append(para_2);
  single_slide.append(para_3);
};
change_background_color = function(single_slide, color = 'white') {
  single_slide.style = "background-color:" + color;
};


// Main code
var slides = document.getElementsByTagName("slide");
var headers = document.getElementsByTagName("h2");

// Handle title slide
add_logo(slides[0])
document.getElementsByClassName("auto-fadein")[0].style = 'left: 70%';

// Handle normal slide
for (i = 1; i < slides.length; i++) {
  add_header(slides[i]);
  add_footer(slides[i]);
  change_background_color(slides[i], 'white');
}
for (i = 1; i < headers.length; i++) {
  if (headers[i].innerText !== "") {
    add_logo(slides[i]);
    add_header_underline(headers[i]);
  }
}
