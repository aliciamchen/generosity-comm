function instructions(condition_number) {
    return {
        type: jsPsychInstructions,
        pages: [
            instructionsPage1(condition_number),
            instructionsPage2()
        ],
        show_clickable_nav: true,
        show_page_number: true
    }
}

function instructionsPage1(condition_number) {
    return `
    <h2>Instructions</h2>
    <p>
    In this survey, you will read about <strong>${fetchTrialParams(condition_number).length}</strong> scenarios, each one describing a social interaction between two people.
    </p>
    <p>
    For each scenario, we will <strong>ask you about what is likely to happen the next time the social interaction occurs.</strong>
    </p>
    <p>
    You will see three choices for each scenario and be asked to rate how likely each choice is. Please consider each choice <strong>independently</strong> from the other two choices.
    </p>
    `
}

function instructionsPage2() {
    return `
    <h2>Instructions</h2>
    <h3>Relationships</h3>
    <br>
    <p>
    For each scenario, we will tell you about the relationship between the two people.
    </p>
    <p style="color: blue;">Please read all of the scenarios carefully. Make sure to pay attention to the relationship! 🙂</p>
    <p>
    You will receive $${params.basePay} if you successfully complete this study.
    </p>
    <p style="color: red;">
    ⚠️ Press 'next' to begin the study. ⚠️
    </p>
    `
}
