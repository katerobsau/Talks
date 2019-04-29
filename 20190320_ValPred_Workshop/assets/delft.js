function createElement(tag0, class0) {
  let newEL = document.createElement(tag0);
  newEL.classList.add(class0);
  return newEL;
}

function addTitleColorBanners(slideDOM) {
  let newClasses = [1,2,3,4,5].map(x => "title-header-" + x);
  newClasses.map(x => createElement("div", x))
            .forEach(x => slideDOM.prepend(x));
}

function addTitleBackground(slideDOM) {
  let imgDOM = createElement("img");
  imgDOM.src = "assets/title_background2.jpg";
  imgDOM.classList.add("title-background");
  slideDOM.prepend(imgDOM);
}

function addTitleLogoText(slideDOM) {
  let imgDOM = createElement("img");
  imgDOM.src = "assets/title_logo.png";
  imgDOM.classList.add("title-logo-text");
  slideDOM.appendChild(imgDOM);
}

function addSlideLogoText(slideDOM) {
  let imgDOM = createElement("img");
  imgDOM.src = "assets/slide_logo.png";
  imgDOM.classList.add("slide-logo-text");
  slideDOM.appendChild(imgDOM);
}

function addSlideLogoFlame(slideDOM) {
  let imgDOM = createElement("img");
  imgDOM.src = "assets/slide_flame_transparent.gif";
  imgDOM.classList.add("slide-logo-flame");
  slideDOM.appendChild(imgDOM);
}

function addColorBanners(slideDOM) {
  let newClasses = ["footer-banner"];
  newClasses.map(x => createElement("div", x))
            .forEach(x => slideDOM.appendChild(x));
}

function changeBackgroundColor(slideDOM) {
  slideDOM.style = "background-color: 'white';";
}

function addUnderline(h2DOM) {
  if (h2DOM.innerText !== "") {
    h2DOM.classList.add("header-underline");
  }
}

// Apply theme to slides =================================================
function addTheme(doc) {

  let titleSlide = doc.querySelector(".title-slide");
  addTitleColorBanners(titleSlide);
  addTitleBackground(titleSlide);
  addTitleLogoText(titleSlide);

  let allSlides = doc.querySelectorAll("slide:not(:first-child)");
  allSlides.forEach(addColorBanners);
  allSlides.forEach(changeBackgroundColor);
  allSlides.forEach(addSlideLogoText);
  allSlides.forEach(addSlideLogoFlame);

  let h2DOM = doc.querySelectorAll("h2");
  h2DOM.forEach(addUnderline);
}

addTheme(document);
