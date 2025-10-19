// This react component just wraps the experiment to dispay them properly.
// The experiment themselecs is located in experiment_*.js

// save data, reaction time problem, styles in general, mobile data
// what;s up with the versions!

import React from 'react';

import ExperimentWrapper from '../../components/ExperimentWrapper';

// import './styles.css';

import experimentInfo from './info';
import jsPsychExperiment from './experiment';

class Experiment extends React.Component {
  render() {
    return (
      <ExperimentWrapper experimentInfo={experimentInfo} jsPsychExperiment={jsPsychExperiment} {...this.props} />
    )
  }
}

export default Experiment;
