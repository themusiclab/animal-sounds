
import { ParameterType } from "jspsych";
import style from './styles.css';

const info = {
  name: "animal-sounds",
  parameters: {
    mo: {
      type: ParameterType.BOOLEAN,
      default: false,
    },
    image_1: {
      type: ParameterType.IMAGE,
      default: undefined,
    },
    audio_1: {
      type: ParameterType.AUDIO,
      default: undefined,
    },
    image_2: {
      type: ParameterType.IMAGE,
      default: undefined,
    },
    audio_2: {
      type: ParameterType.AUDIO,
      default: undefined,
    },
    order: {
      type: ParameterType.STRING,
      default: undefined,
    },
    image_animation: {
      type: ParameterType.STRING,
      default: 'jump',
    },
    sound_animation: {
      type: ParameterType.STRING,
      default: 'flash',
    },
    listen_instr: {
      type: ParameterType.STRING,
      default: 'Listen to both songs',
    },
    answer_instr: {
      type: ParameterType.STRING,
      default: 'Choose a song',
    },
    progress: {
      type: ParameterType.INTEGER,
      default: -1,
    }
  },
}

class jsPsychAnimalSounds {
  constructor(jsPsych) {
    this.jsPsych = jsPsych;
  }

  async trial(display_element, trial) {
    /**
     * **animal-sounds**
     *
     * Plays 2 sounds produced by two animals, lets the user
     * choose the one they prefer.
     *
     */

    display_element.innerHTML = ''

    var trial_data = {}
    var selection_start_time

    // Get important screen dimensions and divisions
    var div_ppn = 0.6
    var s = { h: document.body.clientHeight, w: document.body.clientWidth } // Just for brevity
    s.m = Math.min(s.w, s.h)
    //var btn_ppn = s.h * (1-divn_ppn);  What ISAAC wrote
    var btn_ppn = s.h * (-1)
    var img_ppn = s.h * div_ppn

    // Containers for nodes on either side of the screen
    var left_side = []
    var right_side = []

    // Load audio
    var audio_ended = false
    var context = this.jsPsych.pluginAPI.audioContext()
    var audio_1
    var audio_2
    var audio_1_loaded = false
    var audio_2_loaded = false

    console.log("Trial", trial)

    // ToDo: Use await here w/ promises
    try {
      const buffer = await this.jsPsych.pluginAPI.getAudioBuffer(trial.audio_1)
      if (context !== null) {
        audio_1 = context.createBufferSource()
        audio_1.buffer = buffer
        audio_1.connect(context.destination)
      } else {
        audio_1 = buffer
        audio_1.currentTime = 0
      }
      audio_1_loaded = true
      if (audio_1_loaded & audio_2_loaded) {
        ctrl_fcn('start')
      }
    } catch (excpetion) {
      console.error(
        `Failed to load audio file "${trial.audio_1}". Try checking the file path. We recommend using the preload plugin to load audio files.`
      )
      console.error(excpetion)
    }

    try {
      const buffer_2 = await this.jsPsych.pluginAPI.getAudioBuffer(trial.audio_2)
      if (context !== null) {
        audio_2 = context.createBufferSource()
        audio_2.buffer = buffer_2
        audio_2.connect(context.destination)
      } else {
        audio_2 = buffer_2
        audio_2.currentTime = 0
      }
      audio_2_loaded = true
      if (audio_1_loaded & audio_2_loaded) {
        ctrl_fcn('start')
      }
    } catch (excpetion) {
      console.error(
        `Failed to load audio file "${trial.audio_2}". Try checking the file path. We recommend using the preload plugin to load audio files.`
      )
      console.error(excpetion)
    }

    // Prep buttons
    /*
    var btn = document.createElement('button');
    btn.className = 'jspsych-btn';
    btn.style.position = 'absolute';
    btn.style.visibility = 'hidden';
    btn.disabled = true;
    btn.style.borderRadius = '50%'; // make circular
    var btn_diam = Math.round(0.8 * btn_ppn);
    btn.style.height = btn_diam + 'px';
    btn.style.width = btn_diam + 'px';
    btn.style.fontSize = Math.round(s.m/15) + 'px';
    btn.style.top = (img_ppn + btn_ppn/2 - btn_diam/2)  + 'px';
    var btn1 = btn.cloneNode();
    var btn2 = btn.cloneNode();
    btn1.innerText = 'A';
    btn2.innerText = 'B';
    btn1.style.left = Math.round(s.w/4 - btn_diam/2) + 'px';
    btn2.style.left = Math.round(3*s.w/4 - btn_diam/2) + 'px';
    display_element.appendChild(btn1);
    display_element.appendChild(btn2);
    left_side.push(btn1);
    right_side.push(btn2);
    */
    function selection_made(key) {
      trial_data.rt = Date.now() - selection_start_time
      trial_data.selection = key
      ctrl_fcn('end trial')
    }

    // Prep labels
    var btn = document.createElement('button')
    // btn.className = 'jspsych-btn';
    btn.style.position = 'absolute'
    btn.style.visibility = 'hidden'
    // btn.disabled = true;
    // btn.style.borderRadius = '50%'; // make circular
    var btn_diam = Math.round(0.8 * btn_ppn)
    // btn.style.height = btn_diam + 'px';
    // btn.style.width = btn_diam + 'px';
    btn.style.fontSize = Math.round(s.m / 15) + 'px'
    btn.style.top = (img_ppn + btn_ppn / 2 - btn_diam / 2) + 'px'
    btn.style.width = (s.w / 4) + 'px'
    btn.style.textAlign = 'center'
    btn.classList.add(style.FJBack); //Can't figure out how to make it not span the full width
    var btn1 = btn.cloneNode()
    var btn2 = btn.cloneNode()

    if (trial.mo) {
      btn1.innerText = 'Left'
      btn2.innerText = 'Right'
    } else {
      btn1.innerText = 'F'
      btn2.innerText = 'J'
    }
    display_element.appendChild(btn1)
    display_element.appendChild(btn2)
    // btn1.style.left = Math.round(s.w/4 - btn.clientWidth/2) + 'px';
    btn1.style.left = (s.w / 8) + 'px'
    // btn2.style.left = Math.round(3*s.w/4 - btn.clientWidth/2) + 'px';
    btn2.style.left = (s.w / 2) + (s.w / 8) + 'px'
    left_side.push(btn1)
    right_side.push(btn2)


    // ADD PROGRESS BAR
    // create the progress bar container
    if (trial.progress !== -1) {
      const progressBarContainer = document.createElement('div');
      progressBarContainer.id = style.progressBarContainer;
      progressBarContainer.className = style.progressBarContainer;

      // create the progress bar
      const progressBar = document.createElement('div');
      progressBar.id = style.progressBar;
      progressBar.className = style.progressBar;

      // append the progress bar to the container
      progressBarContainer.appendChild(progressBar);
      display_element.appendChild(progressBarContainer);

      function updateProgressBar(percentage) {
        let progressBar = document.getElementById(style.progressBar);
        progressBar.style.width = percentage + '%';
      }

      // update value
      updateProgressBar(trial.progress);
    }
    
    // Prep instructions
    var p = document.createElement('p')
    p.innerHTML = trial.listen_instr
    display_element.appendChild(p)

    // Prep canvases for images
    var cnv = document.createElement('canvas')
    cnv.style.visibility = 'hidden'
    cnv.style.position = 'absolute'
    var cnv_diam = 0.7 * Math.min(s.w / 2, img_ppn)
    cnv.height = cnv_diam
    cnv.width = cnv_diam
    cnv.style.top = (img_ppn / 2 - cnv.height / 2) + 'px'
    var icnv1 = cnv.cloneNode()
    var icnv2 = cnv.cloneNode()
    icnv1.style.left = (s.w / 4 - icnv1.width / 2) + 'px'
    icnv2.style.left = (s.w * 3 / 4 - icnv2.width / 2) + 'px'
    display_element.appendChild(icnv1)
    display_element.appendChild(icnv2)
    var ictx1 = icnv1.getContext('2d')
    var ictx2 = icnv2.getContext('2d')
    ictx2.translate(icnv2.width, 0)
    ictx2.scale(-1, 1)
    left_side.push(icnv1)
    right_side.push(icnv2)

    // Prep images

    var img1 = new Image()
    var img2 = new Image()
    function img2ctx(img, cnv, ctx, dx, dy) {
      ctx.clearRect(0, 0, cnv.width, cnv.height)
      var rsf = Math.min(0.8 * cnv.width / img.width, 0.8 * cnv.height / img.height)
      ctx.drawImage(
        img,
        cnv.width / 2 - rsf * img.width / 2 + dx,
        cnv.height / 2 - rsf * img.height / 2 + dy,
        rsf * img.width,
        rsf * img.height
      )
    }
    img1.onload = function () { img2ctx(img1, icnv1, ictx1, 0, 0) }
    img2.onload = function () { img2ctx(img2, icnv2, ictx2, 0, 0) }
    img1.src = trial.image_1
    img2.src = trial.image_2

    // Prep canvases for animations
    var cnv = document.createElement('canvas')
    cnv.style.visibility = 'hidden'
    cnv.style.position = 'absolute'
    var cnv_diam = Math.min(s.w / 2, img_ppn)
    cnv.height = cnv_diam
    cnv.width = cnv_diam
    cnv.style.top = (img_ppn / 2 - cnv.height / 2) + 'px'
    var acnv1 = cnv.cloneNode()
    var acnv2 = cnv.cloneNode()
    acnv1.style.left = (s.w / 4 - acnv1.width / 2) + 'px'
    acnv2.style.left = (s.w * 3 / 4 - acnv2.width / 2) + 'px'
    display_element.appendChild(acnv1)
    display_element.appendChild(acnv2)
    left_side.push(acnv1)
    right_side.push(acnv2)
    var actx1 = acnv1.getContext('2d')
    var actx2 = acnv2.getContext('2d')
    actx2.translate(acnv2.width, 0)
    actx2.scale(-1, 1)

    // Prepare animation functions
    var anim_durat = 3000 // 3-second animations
    function restore_img(curr, ur) {
      var props = ['top', 'left']
      var i, prop
      for (i = 0; i < props.length; i++) {
        prop = props[i]
        curr.style[prop] = ur.style[prop]
      }
    }
    function image_animation(which, img, cnv, ctx, start_time) {
      var elapsed = Date.now() - start_time
      var dx = 0
      var dy = 0
      var max_disp = 0.1 * Math.min(cnv.width, cnv.height)
      if (!audio_ended) {
        if (which == 'jump') {
          dy = -max_disp * Math.abs(Math.sin((elapsed) / 120))
        } else if (which == 'slide') {
          dx = 0.5 * max_disp * Math.sin((elapsed) / 60)
        }
      }
      img2ctx(img, cnv, ctx, dx, dy)
      if (!audio_ended) {
        window.requestAnimationFrame(function () {
          image_animation(which, img, cnv, ctx, start_time)
        })
      }
    }
    function sound_animation(which, cnv, ctx, start_time) {
      var curr_time = Date.now()
      var elapsed = curr_time - start_time
      ctx.clearRect(0, 0, cnv.width, cnv.height)
      if (which == 'grow') {
        var n_lines = Math.floor(elapsed / 120) + 1
        n_lines = ((n_lines % 4) + 4) % 4 // modulo
        var i
        for (i = 1; i <= n_lines; i++) {
          ctx.beginPath()
          ctx.arc(cnv.width / 2, cnv.height / 2, cnv.height / 2 * 0.7 + i * cnv.height / 2 * 0.1, 1.6 * Math.PI, 1.9 * Math.PI)
          ctx.stroke()
        }
      } else if (which == 'flash') {
        var show = Math.floor(elapsed / 300)
        show = ((show % 2) + 2) % 2 // modulo
        if (show == 0) {
          var n_lines = 3
          var i
          for (i = 1; i <= 3; i++) {
            ctx.beginPath()
            ctx.arc(cnv.width / 2, cnv.height / 2, cnv.height / 2 * 0.7 + i * cnv.height / 2 * 0.1, 1.6 * Math.PI, 1.9 * Math.PI)
            ctx.stroke()
          }
        }
      }
      if (audio_ended) {
        ctx.clearRect(0, 0, cnv.width, cnv.height)
      } else {
        window.requestAnimationFrame(function () {
          sound_animation(which, cnv, ctx, start_time)
        })
      }
    }
    function set_vis(elements, state) {
      var i
      for (i = 0; i < elements.length; i++) {
        elements[i].style.visibility = state
      }
    }

    // Control flow for the trial
    let _plugin_this = this
    function ctrl_fcn(ctrl) {
      if (ctrl == 'start') {
        setTimeout(
          function () {
            ctrl_fcn('play left')
          },
          500 //This is the amount of time before the first song plays
        )
      } else if (ctrl == 'play left') {
        audio_ended = false
        audio_1.addEventListener(
          'ended',
          function () {
            audio_ended = true
            setTimeout(
              function () {
                ctrl_fcn('play right')
              },
              1000 //This is the amount of time between songs
            )
          }
        )
        set_vis(right_side, 'hidden')
        set_vis(left_side, 'visible')
        image_animation(trial.image_animation, img1, icnv1, ictx1, Date.now())
        sound_animation(trial.sound_animation, acnv1, actx1, Date.now())
        if (context !== null) {
          audio_1.start(context.currentTime)
        } else {
          audio_1.play()
        }
      } else if (ctrl == 'play right') {
        audio_ended = false
        audio_2.addEventListener(
          'ended',
          function () {
            audio_ended = true
            setTimeout(
              function () {
                ctrl_fcn('begin selection')
              },
              100 // Time after the second song
            )
          }
        )
        set_vis(left_side, 'hidden')
        set_vis(right_side, 'visible')
        image_animation(trial.image_animation, img2, icnv2, ictx2, Date.now())
        sound_animation(trial.sound_animation, acnv2, actx2, Date.now())
        if (context !== null) {
          audio_2.start(context.currentTime)
        } else {
          audio_2.play()
        }
      } else if (ctrl == 'begin selection') {
        set_vis(left_side, 'visible')
        set_vis(right_side, 'visible')
        p.innerHTML = trial.answer_instr
        // Add clickable divs
        var d = document.createElement('div')
        d.style.position = 'absolute'
        d.style.top = '0px'
        d.style.height = (s.h) + 'px'
        d.style.width = (s.w / 2) + 'px'
        var d1 = d.cloneNode()
        var d2 = d.cloneNode()
        d1.style.left = '0px'
        d2.style.left = (s.w / 2) + 'px'
        d1.addEventListener('click', function () { selection_made('F') })
        d2.addEventListener('click', function () { selection_made('J') })
        display_element.appendChild(d1)
        display_element.appendChild(d2)
        /*
        btn1.onclick = function() {
          selection_made('A');
        }
        btn2.onclick = function() {
          selection_made('B');
        }
        */
        // btn1.disabled = false;
        // btn2.disabled = false;
        selection_start_time = Date.now()
        document.onkeydown = function (info) {
          var key = info.key.toUpperCase()
          if (key == 'F' || key == 'J') {
            document.onkeydown = null
            selection_made(key)
          }
        }
      } else if (ctrl == 'end trial') {
        var i, k
        var props = ['image_1', 'image_2', 'audio_1', 'audio_2', 'order']
        for (i = 0; i < props.length; i++) {
          k = props[i]
          trial_data[k] = trial[k]
        }
        // end trial
        _plugin_this.jsPsych.finishTrial(trial_data)
      }
    }
  }
}

jsPsychAnimalSounds.info = info;

export default jsPsychAnimalSounds;
