$(document).ready(function() {
  console.log("hallo welt")

  Shiny.addCustomMessageHandler(
    "qz-update-player-card",
    (message) => {
      console.log(message);

      message.forEach((player) => {
        let id = player.id[0];
        let score = player.score[0];

        document.getElementById(id).querySelector(".qz-player-card__score").innerText = score;
      })
    }
  )
})
