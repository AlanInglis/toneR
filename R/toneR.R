#' toneR
#'
#' @description Play a tone
#'
#'
#' @param sound A number or name (character) of the tone to be played.
#' If \code{sound} does not match any of the inbuilt tones, a random tone will
#' be played.
#'
#' @importFrom stringr str_trim
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace_all
#' @importFrom audio play
#' @importFrom audio load.wave
#' @import utils
#'
#' @return NULL
#'
#' @examples
#' # Play "vader" breathing sound
#' tone("vader")
#'
#' @export



tone <- function(sound = 1) {

  # set all sounds
  sounds <- c(
    chewy = "chewy.wav",
    doh = "doh.wav",
    hyperdrive_fail = "hyperdrive_fail.wav",
    lightsaber = "lightsaber.wav",
    R2D2_1 = "R2D2_1.wav",
    R2D2_2 = "R2D2_2.wav",
    R2D2_3 = "R2D2_3.wav",
    TIE_fire = "TIE_fire.wav",
    TIE_fly = "TIE_fly.wav",
    TNG_alarm = "TNG_alarm.wav",
    TNG_alarm2 = "TNG_alarm2.wav",
    TNG_alarm3 = "TNG_alarm3.wav",
    TNG_communicator = "TNG_communicator.wav",
    TNG_computer = "TNG_computer.wav",
    TNG_deny = "TNG_deny.wav",
    TNG_phaser = "TNG_phaser.wav",
    TNG_powerloss = "TNG_powerloss.wav",
    TNG_torpedo = "TNG_torpedo.wav",
    TNG_tractor = "TNG_tractor.wav",
    TNG_transporter = "TNG_transporter.wav",
    TOS_communicator = "TOS_communicator.wav",
    TOS_phaser = "TOS_phaser.wav",
    TOS_transporter = "TOS_transporter.wav",
    vader = "vader.wav",
    purge  = "purge.wav",
    facebook = "facebook.wav",
    whatsapp = "whatsapp.wav",
    iphone   = "iphone.wav")

  # set sound path
  sound_path <- NULL


  if (is.na(sounds[sound]) || length(sounds[sound]) != 1) {
    if (is.character(sound)) {
      # trim whitespace from string
      sound <- str_trim(sound)
      # set the chosen sound
      if (file.exists(sound)) {
        sound_path <- sound
      } else {
        warning(paste('"', sound, '" is not a tone found in toneR. Playing a random tone instead.', sep = ""))
      }
    }
  } else {
    sound_path <- system.file(paste("sounds/", sounds[sound], sep = ""), package = "toneR")
  }

  # If sound doesn't exist, play a random sound
  if (is.null(sound_path)) {
    sound_path <- system.file(paste("sounds/", sample(sounds, size = 1), sep = ""), package = "toneR")
  }

  # error message
  tryCatch(playFile(sound_path), error = function(er) {
    warning("tone() could not play the sound due to the following error:\n", er)
  })
}


# Detect the presence or absence of a pattern in a string.
isWavName <- function(name) {
  str_detect(name, regex("\\.wav$", ignore_case = TRUE))
}

replaceSpaces <- function(s) {
  str_replace_all(s, " ", "\\\\ ")
}


# play VLC
playVLC <- function(name) {
  name <- replaceSpaces(name)
  system(paste0("vlc -Idummy --no-loop --no-repeat --playlist-autostart --no-media-library --play-and-exit ", name),
    ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE
  )
  invisible(NULL)
}

# play Pulse Audio (LINUX)
playPaplay <- function(name) {
  name <- replaceSpaces(name)
  system(paste0("paplay ", name), ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE)
  invisible(NULL)
}

# play ALSA Audio
playAplay <- function(name) {
  name <- replaceSpaces(name)
  system(paste0("aplay --buffer-time=48000 -N -q ", name), ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE)
  invisible(NULL)
}


# play the sound
playAudio <- function(name) {
  soundWav <- load.wave(name)
  play(soundWav)
}


# play the file
playFile <- function(name) {
  if (Sys.info()["sysname"] == "Linux") {
    if (isWavName(name) && nchar(Sys.which("paplay")) >= 1) {
      playPaplay(name)
    } else if (isWavName(name) && nchar(Sys.which("aplay")) >= 1) {
      playAplay(name)
    } else if (nchar(Sys.which("vlc")) >= 1) {
      playVLC(name)
    } else {
      playAudio(name)
    }
  } else {
    playAudio(name)
  }
}
