//////////////////
// 1. IMPORTS  //
//////////////////


import { bird_list, frog_list, insect_list, mammal_list } from './stim-list.js'
import jsPsychAnimalSounds from './animalSounds_plugin';
import s from './styles.css';

import { init } from "../common/centralJsPsych";

import jsPsychHtmlKeyboardResponse from "@jspsych/plugin-html-keyboard-response";
import jsPsychHtmlButtonResponse from "@jspsych/plugin-html-button-response";
import jsPsychAudioKeyboardResponse from "@jspsych/plugin-audio-keyboard-response";
import jsPsychPreload from "@jspsych/plugin-preload";
import '../common/custom-plugins/jspsych-react.js';

import isMobile from '../common/isMobile';
import experimentInfo from './info';
import api from '../common/api';
import baseUrl from '../../core/baseUrl';

import {
	intro,
	musicxp,
	workspace,
	// musicTalent,
	auditory_imagery,
	visual_imagery,
	demog,
	email,
	// genomicsItems,
	mgP17Items
} from '../common/covariates';

import {
	calibrationAudioPreload,
	getAutoHeadphoneCheck
  } from '../common/calibration';

///////////////////////////////
// 2. EXPERIMENT PARAMETERS //
///////////////////////////////


const mobile = isMobile();

// Modify the URLs in all stimuli (by reference)
// to use the full URLs incl. the baseUrl at the beginning
[
	...mammal_list,
	...frog_list,
	...insect_list,
	...bird_list
].map((stim) => {
	stim.Sound1 = `${baseUrl}/quizzes/havoc/audio/${stim.Sound1}.mp3`
	stim.Sound2 = `${baseUrl}/quizzes/havoc/audio/${stim.Sound2}.mp3`
})

function winTextAssembler(img, img_graph, win_text) {
	const img_src = `${baseUrl}/quizzes/havoc/img/${img}`
	const img_graph_src = `${baseUrl}/quizzes/havoc/img/${img_graph}`
	const html_out = `<div class=${s.imgBox}><img class=${s.centerFit} src=${img_src}></div><br><p class=${s.whiteBack}>${win_text}</p><br/><div class=${s.imgBox}><img class=${s.centerFit} src=${img_graph_src}></div>`
	return html_out
}

const winner_text = {
	'Bird': winTextAssembler('birdWinner.png', 'Bird_Graph.jpg', 'Birds are highly social creatures, and they use their songs to recognize each other, attract potential mates, and defend their territories. Your brain may have picked up on the subtle differences between songs that can make one song sound more beautiful to other birds! Differences in the <b><i>complexity</b></i> or <b><i>difficulty</b></i> of sounds can be particularly important for birds like the <b>prairie warbler</b> from North America and <b>zebra finch</b> from Australia.'),
	'Insect': winTextAssembler('insectWinner.png', 'Insect_Graph.jpg', 'Insects produce a marvelous array of sounds. For example, <b>Pacific field crickets</b> in Hawaii and <b>bow-winged grasshoppers</b> in Europe chirp at night to attract mates. Your brain may have picked up on important aspects of <b><i>pitch</i></b> and <b><i>timing</i></b> that are key to how beautiful a song is. Amazingly, a species of <b>tiger moth</b> can produce ultrasonic chirps that confuse echolocating bats that are hunting them!'),
	'Frog': winTextAssembler('frogWinner.png', 'Frog_Graph.jpg', 'Frogs produce many types of sounds, and most species of frog have a unique call to attract mates. For instance, the <b>hourglass frog</b> from Central and South America makes a “creek” sound, the <b>green tree frog</b> from North America makes a higher pitched “croak” sound, and the <b>túngara</b> frog from Central and South America makes a sound like a laser gun! Your brain may have noticed the <b><i>harmonics</b></i> and <b><i>tempo</b></i> of these calls that are known to be important for frog communication.'),
	'Mammal': winTextAssembler('mammalWinner.png', 'Mammal_Graph.jpg', 'Mammals, including humans, use a diversity of sounds to communicate. The <b>gelada</b>, a monkey from Ethiopia, uses grunts and hoots to attract mates. The <b>singing mouse</b> from Central America sings a high pitched trill to attract mates. Your brain may have been attuned to the <b><i>diversity</i></b> of sound qualities, which is known to be important for many species of mammals.')
}

const share_emojis = {
	'Bird': '%F0%9F%8E%B6%F0%9F%90%A6%F0%9F%A6%9C%F0%9F%A6%85%0A',
	'Insect': '%F0%9F%8E%B6%F0%9F%A6%97%F0%9F%AA%B0%F0%9F%A6%8B',
	'Frog': '%F0%9F%8E%B6%F0%9F%90%B8%F0%9F%90%B8%F0%9F%90%B8',
	'Mammal': '%F0%9F%8E%B6%F0%9F%A6%8D%F0%9F%90%81%F0%9F%A6%8C'
}

// list of prompts listener gets to frame their comparison of vocalizations - now just using LIKE
const listenList = ['<b>like</b> more?'];

// trials per category
const trials_per_category = 4;
const max_bonus_trials_per_category = 10;
const bonus_trials = []
bonus_trials.flag = false;


//////////////////////////////////
// 3. EXPORTING startExperiment //
//////////////////////////////////


export default function startExperiment(options) {

	/////////////////////
	// 4. INIT JsPsych //
	/////////////////////


	const background = document.querySelector('main');

	if (background) {
		background.classList.add(`${s.havocBackground}`);
	}

	const jsPsych = init({
		timeline: timeline,
		preload_audio: calibrationAudioPreload,
		use_webaudio: true,
		display_element: options.targetElement,
		on_trial_finish: (data) => {
			api.saveDataOnFinish(data);
			options.targetElement.focus()

			// Reset to top of page
			window.scrollTo(0, 0)
		}
	})
	api.init(experimentInfo, options)


	////////////////////
	// 5. TRIAL LISTS //
	////////////////////


	function randomizeSoundOrder(arr) {
		return arr.map(item => {
			if (Math.random() > 0.5) {
				return {
					...item,
					Sound1: item.Sound2,
					Sound2: item.Sound1
				};
			}
			return item;
		});
	}

	function generate_trials(n_per_category) {
		const mammal_stim = jsPsych.randomization.sampleWithReplacement(mammal_list, n_per_category)
		const frog_stim = jsPsych.randomization.sampleWithReplacement(frog_list, n_per_category)
		const insect_stim = jsPsych.randomization.sampleWithReplacement(insect_list, n_per_category)
		const bird_stim = jsPsych.randomization.sampleWithReplacement(bird_list, n_per_category)

		// Combine stimuli into one list
		const stim_list = mammal_stim.concat(frog_stim, insect_stim, bird_stim)

		// Generate list of files to preload
		const audioPreload = [
			...stim_list.map((stim) => stim.Sound1),
			...stim_list.map((stim) => stim.Sound2)
		]

		// Shuffle stims
		let trial_stim = jsPsych.randomization.shuffle(stim_list)

		// Shuffle order of the sounds
		trial_stim = randomizeSoundOrder(trial_stim);

		const trial_list = trial_stim.map((trial, idx) => {
			return {
				...trial,
				listen_instr: `<p id=${s.callText}>Which call do you ${jsPsych.randomization.sampleWithReplacement(listenList, 1)}</p>`,
				answer_instr: `<p id=${s.callText}>Which call do you ${jsPsych.randomization.sampleWithReplacement(listenList, 1)}</p>`,
				image_1: `${baseUrl}/quizzes/havoc/img/${trial.Species + '.png'}`,
				image_2: `${baseUrl}/quizzes/havoc/img/${trial.Species + '.png'}`,
				image_animation: jsPsych.randomization.sampleWithReplacement(['jump', 'slide'], 1),
				sound_animation: jsPsych.randomization.sampleWithReplacement(['grow', 'flash'], 1),
				progress: (idx / trial_stim.length) * 100
			};
		});

		const imagePreload = [
			...trial_list.map((stim) => stim.image_1),
			...trial_list.map((stim) => stim.image_2)
		];

		return {audioPreload, imagePreload, trial_list}
	}

	const {audioPreload, imagePreload, trial_list} = generate_trials(trials_per_category);
	const {audioPreload: audioPreload2, imagePreload: imagePreload2, trial_list:trial_list2 } = generate_trials(max_bonus_trials_per_category);

	console.log(trial_list);

	///////////////////////////
	// 6. TIMELINE VARIABLES //
	///////////////////////////

	const ethics = {
        type: jsPsychHtmlButtonResponse,
        stimulus: `<div align="left" class="${s.whiteBack2}">` +
            'This citizen-science experiment is being conducted by researchers at Yale University. <b>Before you decide to participate, please read this.</b>' +
            '<br><br>' +
            'We study how people make sense of the music, speech, and other sounds they hear. We will play you some sounds. We will ask you questions about what you hear. ' +
            '<br><br>' +
            'We will also ask you questions concerning your emotions and behaviors, your preferences and beliefs about music and the arts, your engagement with musical activities, your personal history of musical exposure and training, and some demographic information about you and your family. ' +
            '<br><br>' +
            'The experiment has no known risks or anticipated direct benefits. Your participation is voluntary, you will not receive compensation, you can stop at any time without penalty, and your participation is completely anonymous. No names or any other identifying information will be collected. The results and data will be shared publicly but no one will know that you participated. After the experiment, we will explain several ways to stay informed about the research. If you have questions or problems, you can contact us at musiclab@yale.edu. ' +
            '<br><br>' +
            '<b>We recommend that you wear headphones.</b> If you don’t have any, you can use speakers instead. By proceeding, you agree to participate.' +
            '<br><br><small>' +
            'Title: Citizen-science approaches to auditory perception. PI: Samuel Mehr. Contact: musiclab@yale.edu. IRB Protocol: #2000033433. If you have questions about your rights as a research participant, or you have complaints about this research, call the Yale Institutional Review Board at (203) 785-4688 or email hrpp@yale.edu.' +
			'<br><div style="text-align: center;">---</div>' +
			'This citizen-science experiment is part of a collaborative project between researchers at McGill University, the University of Auckland, and Yale University. You can contact Sarah Woolley (sarah.woolley@mcgill.ca) with questions about the collaboration. Participants in this experiment must be 18 years of age or older.' +
            '<br><div style="text-align: center;">---</div>' +
			'</small>' +
			'<small>We thank Elizabeth Adkins-Regan, Jaime Bosch, Katherine Buchanan, Bruce Byers, Morgan Gustison, Albertine Leitão, Stefan Leitner, Stephen Nowicki, Bret Pasch, Susan Peters, Michael Reichert, Michael Ritchie, Michael Ryan, Ryan Taylor, Robin Tinghitella, Michelle Tomaszycki, Eric-Marie Vallet, Sarah Woolley, and the Macaulay Library for generously providing acoustic stimuli. The background image was designed by pikisuperstar, available at Freepik. Animal images were obtained from phylopic.org.</small>' +
            '</div><br>',
        prompt1: ' ',
        prompt2: ' ',
        choices: ['Next'],
        on_finish: () => {
            api.onStartExperiment();
        }
    };


	let introText
	if (mobile) {
		introText = `<div class="${[s.whiteBack, s.mb2].join(' ')}"><p>For each animal, you will listen to two calls and decide which you prefer. You will respond by either tapping the left or right side of the screen.</p></div>`
	} else {
		introText = `<div class="${[s.whiteBack, s.mb2].join(' ')}"><p>For each animal, you will listen to two calls and decide which you prefer. Press the F or J keys or click with the mouse to make your selection.</p><img style="height:20vh;" src="${baseUrl}/quizzes/havoc/img/fj_hands_animate.gif" alt="Keyboard with the F and J keys highlighted in yellow"/></div>`
	}

	const welcome = {
		type: jsPsychHtmlButtonResponse,
		stimulus: `<img class="${s.centerFit}" src="${baseUrl}/quizzes/havoc/img/frontPage.png"><div class="${[s.whiteBack, s.mb2].join(' ')}"><p style="font-size:1.5em;">Many animals use sounds to attract others.  In this game you'll find out what type of animal <b>YOU</b> match with!</p></div>`,
		choices: [`Next`]
	};

	const instructions = {
		type: jsPsychHtmlButtonResponse,
		stimulus: introText,
		choices: [`Begin`]
	};

	const main_trials = {
		timeline: [
			{
				type: jsPsychAnimalSounds,
				mo: mobile,
				image_1: [jsPsych.timelineVariable('image_1')],
				image_2: [jsPsych.timelineVariable('image_2')],
				audio_1: [jsPsych.timelineVariable('Sound1')],
				audio_2: [jsPsych.timelineVariable('Sound2')],
				order: jsPsych.timelineVariable('Quality'),
				image_animation: [jsPsych.timelineVariable('image_animation')],
				sound_animation: [jsPsych.timelineVariable('sound_animation')],
				listen_instr: [jsPsych.timelineVariable('listen_instr')],
				answer_instr: [jsPsych.timelineVariable('answer_instr')],
				progress: jsPsych.timelineVariable('progress'),
				data: {
					category: jsPsych.timelineVariable('Category'),
					species: jsPsych.timelineVariable('Species')
				}
			}
		],
		timeline_variables: trial_list
	};

	const animalIDer = {
		type: jsPsychHtmlButtonResponse,
		stimulus: 'Do you have a hobby or job where you identify animals by their sounds? For example birdwatching, herping, zookeeping, etc.',
		choices: ['Yes','No']
	};

	const calculating = {
		type: jsPsychAudioKeyboardResponse,
		stimulus: `${baseUrl}/quizzes/havoc/audio/calculating.mp3`,
		choices: "NO_KEYS",
		prompt: `<div class=${s.imgBox}><img class=${s.centerFit} src=${baseUrl}/quizzes/havoc/img/Calculating.png></div>`,
		trial_ends_after_audio: true
	};

	const end_page = {
		type: jsPsychHtmlKeyboardResponse,
		stimulus: '',
		choices: [],
		on_start: (trial) => {
			api.onFinishExperiment();

			/* Display performance */
			const all_data = jsPsych.data.get();

			// Identify unique categories
			const all_categories = all_data.select('category').values;
			const unique_categories = [...new Set(all_categories)];

			let corr_by_cat = unique_categories.map(category => {
				let curr_subset = all_data.trials.filter(item => item.category === category);
				let selections = curr_subset.map(item => item.selection);
				let orders = curr_subset.map(item => item.order);

				let result = curr_subset.reduce((acc, curr, i) => {
					if ((selections[i] === 'F' && orders[i] === 'AB') || (selections[i] === 'J' && orders[i] === 'BA')) {
						acc.n_correct++;
					}
					if (orders[i] === 'AB' || orders[i] === 'BA') {
						acc.n_total++;
					}
					return acc;
				}, { n_correct: 0, n_total: 0 });

				return {
					cat: category,
					ppn_corr: result.n_correct / result.n_total
				};
			});

			let max = corr_by_cat.reduce((prev, current) => {
				return (prev.ppn_corr > current.ppn_corr) ? prev : current;
			}, { ppn_corr: 0, cat: 'none' });

			let max_cat = max.cat;

			if (max_cat === 'none') {
				let catList = ['Bird', 'Mammal', 'Insect', 'Frog'];
				let rand = Math.floor(Math.random() * catList.length);
				max_cat = catList[rand];
			}

			trial.stimulus = `Congratulations!! You matched with ${max_cat}s!</br>`
			trial.stimulus += winner_text[max_cat];

			trial.stimulus += `
			<br><p class=${s.whiteBack}>
			<button id="do-more-trials">Listen to more examples</button><br/>
			or <b>share your result</b>:</p>
			<a target=”_blank” href="https://twitter.com/intent/tweet?text=I%20matched%20with%20${max_cat.toLowerCase()}s!%20${share_emojis[max_cat]}%20Which%20animal%20are%20you?%20https%3A//themusiclab.org/quizzes/havoc/">
				<img width="77" height="63" src="${baseUrl}/quizzes/havoc/img/twitter.png" alt="Share on Twitter">
			</a>
			<a target=”_blank” href="https://www.facebook.com/sharer/sharer.php?u=https%3A//themusiclab.org/quizzes/havoc/">
				<img width="77" height="77" src="${baseUrl}/quizzes/havoc/img/fb.png" alt="Share on Facebook">
			</a>
			<p class=${s.whiteBack}>We thank Elizabeth Adkins-Regan, Jaime Bosch, Katherine Buchanan, Bruce Byers, Morgan Gustison, Albertine Leitão, Stefan Leitner, Stephen Nowicki, Bret Pasch, Susan Peters, Michael Reichert, Michael Ritchie, Michael Ryan, Ryan Taylor, Robin Tinghitella, Michelle Tomaszycki, Eric-Marie Vallet, Sarah Woolley, and the Macaulay Library for generously providing acoustic stimuli. The background image was designed by pikisuperstar, available at Freepik. Animal images were obtained from phylopic.org. Sound Effect from Pixabay.
			</p>
		`
		},
		on_load: () => {
			document.getElementById('do-more-trials').addEventListener('click', () => {
				bonus_trials.flag = true;
				jsPsych.finishTrial();
			})
		}
	};

	const pre_load_bonus_trials = {
		type: jsPsychPreload,
		audio: audioPreload2,
		images: imagePreload2,
		conditional_function: () => {
			return bonus_trials
		}
	};

	const pre_load_main_trials = {
		type: jsPsychPreload,
		audio: audioPreload,
		images: imagePreload,
		conditional_function: () => {
			return main_trials
		}
	};

	const bonus_trials = {
		timeline: [
			{
				type: jsPsychAnimalSounds,
				mo: mobile,
				image_1: [jsPsych.timelineVariable('image_1')],
				image_2: [jsPsych.timelineVariable('image_2')],
				audio_1: [jsPsych.timelineVariable('Sound1')],
				audio_2: [jsPsych.timelineVariable('Sound2')],
				order: jsPsych.timelineVariable('Quality'),
				image_animation: [jsPsych.timelineVariable('image_animation')],
				sound_animation: [jsPsych.timelineVariable('sound_animation')],
				listen_instr: [jsPsych.timelineVariable('listen_instr')],
				answer_instr: [jsPsych.timelineVariable('answer_instr')],
				data: {
					category: jsPsych.timelineVariable('Category'),
					species: jsPsych.timelineVariable('Species')
				}
			}
		],
		timeline_variables: trial_list2,
		conditional_function: () => {
			return bonus_trials
		}
	};

	const transition = {
		type: jsPsychHtmlButtonResponse,
		stimulus: '<p>We have just a few questions for you before we compute your results.</p>',
		choices: ['Continue']
	  };

	// const musgenItems = {
	// 	timeline: genomicsItems.extractItems(
	// 		genomicsItems,
	// 		genomicsItems.sampleFinalItems(genomicsItems.genomic_item_categories, 2, 5)
	// 	)
	// };

	const mgP17 = [mgP17Items.mgP17_d_intro, ...jsPsych.randomization.sampleWithoutReplacement(mgP17Items.allItems)];

	//////////////////////////
	// 7. DEFINING TIMELINE //
	//////////////////////////


	const timeline = [
		welcome,
		pre_load_main_trials,
		ethics,
		intro,
		workspace,
   		getAutoHeadphoneCheck(),
		animalIDer,
		instructions,
		main_trials,
		transition,
		...mgP17,
		// musgenItems,
		musicxp,
		// musicTalent,
		auditory_imagery,
    	visual_imagery,
		demog,
		email,
		calculating,
		end_page,
		pre_load_bonus_trials,
		bonus_trials
	];

	api.checkTimeline(timeline);


	/////////////////////////////////
	// 5. INITIALIZING EXPERIMENT //
	/////////////////////////////////

	jsPsych.run(timeline);

}