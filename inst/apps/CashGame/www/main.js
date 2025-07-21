$(document).ready(function() {

  Shiny.addCustomMessageHandler(
    "qz-update-player-card",
    (message) => {
      message.forEach((player) => {
        let id = player.id[0];
        let score = player.score[0];

        document.getElementById(id).querySelector(".qz-player-card__score").innerText = score;
      })
    }
  )

  Shiny.addCustomMessageHandler(
    "qz-update-mc-options",
    (message) => {
      let id_correct = message.id_correct;
      document.getElementById(id_correct).classList.add("btn-success")

      if (message.id_wrong) {
        let id_wrong = message.id_wrong;
        document.getElementById(id_wrong).classList.add("btn-danger")
      }
    }
  )
})
